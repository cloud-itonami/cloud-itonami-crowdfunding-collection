(ns collectops.governor-test
  (:require [clojure.test :refer [deftest is testing]]
            [collectops.advisor :as advisor]
            [collectops.governor :as sut]
            [collectops.phase :as phase]
            [collectops.store :as store]))

(def ctx {:actor-id "collection-test" :phase 3 :now "2026-09-17T00:00:00Z"})
(def after-window {:actor-id "collection-test" :phase 3 :now "2026-09-24T00:00:00Z"})

(defn- verdict
  ([st request] (verdict st request ctx))
  ([st request c] (sut/check request c (advisor/infer st request) st)))

(defn- rules [v] (set (map :rule (:violations v))))

(defn- clean-store
  "The seed with the two uncollectable pledges already expired, so plans
  over it are well-formed."
  []
  (let [s (store/seed-db)]
    (store/commit-record! s {:op :record-attempt
                             :value {:pledge-id "pl-broken"
                                     :attempt {:outcome :hard-decline
                                               :at "2026-09-17T00:00:00Z"}}})
    (doseq [id ["pl-broken" "pl-hard"]]
      (store/commit-record! s {:op :expire-pledge
                               :value {:campaign-id "cf-collecting" :pledge-id id
                                       :at "2026-09-17T00:00:00Z"}}))
    s))

;; ───────────────────────── all-or-nothing, in code ─────────────────────────

(deftest a-live-campaign-can-never-be-charged
  (testing "this is what all-or-nothing MEANS, rather than what the terms say"
    (let [st (store/seed-db)
          v  (verdict st {:op :plan-collection :campaign-id "cf-live"})]
      (is (true? (:hard? v)))
      (is (contains? (rules v) :not-collectible)))))

(deftest one-unpriceable-pledge-refuses-the-whole-plan
  (testing "charging only the ones we can price is how a backer gets billed an unreproducible amount"
    (let [st (store/seed-db)
          v  (verdict st {:op :plan-collection :campaign-id "cf-collecting"})]
      (is (contains? (rules v) :unresolved-charges))
      (is (some #(re-find #"pl-broken" (str (:detail %))) (:violations v))
          "and the offending pledge is named, not swallowed"))))

(deftest an-empty-plan-is-not-authorised
  (let [st (store/mem-store (assoc (store/demo-data) :pledges {}))
        v  (verdict st {:op :plan-collection :campaign-id "cf-collecting"})]
    (is (contains? (rules v) :empty-plan))))

(deftest a-well-formed-plan-still-escalates
  (let [st (clean-store)
        v  (verdict st {:op :plan-collection :campaign-id "cf-collecting"})]
    (is (false? (:hard? v)) "nothing is wrong with it")
    (is (true? (:high-stakes? v)))
    (is (= :escalate (phase/verdict->disposition v))
        "planning is the moment money leaves; it has an author")))

(deftest the-plan-is-re-derived-from-the-store-not-read-from-the-proposal
  (let [st (store/seed-db)
        lying {:op :plan-collection :campaign-id "cf-collecting" :effect :propose
               :confidence 0.99 :summary "請求計画: 3 件"
               :value {:campaign-id "cf-collecting"
                       :plan {:plan/charges [{:charge/pledge "pl-1" :charge/amount-minor 1}]
                              :plan/total-minor 1 :plan/unresolved []}}}]
    (testing "a proposal carrying a hand-made plan is checked as given, and a
             proposal naming only a campaign is recomputed from the record"
      (is (= #{} (rules (sut/check {} ctx lying st)))
          "the carried plan is what gets governed — and it is also what gets committed")
      (is (contains? (rules (verdict st {:op :plan-collection :campaign-id "cf-collecting"}))
                     :unresolved-charges)
          "while the store's own computation refuses"))))

;; ───────────────────────── attempts ─────────────────────────

(deftest silence-is-not-success
  (let [st (clean-store)
        v  (verdict st {:op :record-attempt :campaign-id "cf-collecting" :pledge-id "pl-1"
                        :patch {:outcome :timeout :at "2026-09-18T00:00:00Z"}})]
    (is (contains? (rules v) :unknown-attempt-outcome))))

(deftest a-recorded-outcome-is-a-fact-and-auto-commits
  (let [st (clean-store)
        v  (verdict st {:op :record-attempt :campaign-id "cf-collecting" :pledge-id "pl-1"
                        :patch {:outcome :ok :at "2026-09-18T00:00:00Z"}})]
    (is (true? (:ok? v)))
    (is (= :commit (phase/verdict->disposition v))
        "refusing to record a decline does not make the decline go away")))

(deftest an-already-collected-pledge-takes-no-further-outcomes
  (let [st (clean-store)]
    (store/commit-record! st {:op :record-attempt
                              :value {:pledge-id "pl-1"
                                      :attempt {:outcome :ok :at "2026-09-18T00:00:00Z"}}})
    (is (contains? (rules (verdict st {:op :record-attempt :campaign-id "cf-collecting"
                                       :pledge-id "pl-1"
                                       :patch {:outcome :ok :at "2026-09-19T00:00:00Z"}}))
                   :pledge-not-chargeable))))

;; ───────────────────────── retries and expiry ─────────────────────────

(deftest a-hard-decline-is-never-retried
  (let [st (store/seed-db)
        v  (verdict st {:op :retry-pledge :campaign-id "cf-collecting" :pledge-id "pl-hard"
                        :patch {:at "2026-09-18T00:00:00Z"}})]
    (is (contains? (rules v) :not-retryable)
        "retrying a hard decline is a fraud signal to the issuer, not persistence")))

(deftest a-retry-past-the-window-is-refused
  (let [st (store/seed-db)]
    (store/commit-record! st {:op :record-attempt
                              :value {:pledge-id "pl-1"
                                      :attempt {:outcome :soft-decline
                                                :at "2026-09-18T00:00:00Z"}}})
    (is (= #{} (rules (verdict st {:op :retry-pledge :campaign-id "cf-collecting"
                                   :pledge-id "pl-1" :patch {:at "2026-09-18T00:00:00Z"}}))))
    (is (contains? (rules (verdict st {:op :retry-pledge :campaign-id "cf-collecting"
                                       :pledge-id "pl-1" :patch {:at "2026-09-24T00:00:00Z"}}
                                  after-window))
                   :not-retryable))))

(deftest an-early-drop-is-refused
  (let [st (store/seed-db)]
    (store/commit-record! st {:op :record-attempt
                              :value {:pledge-id "pl-1"
                                      :attempt {:outcome :soft-decline
                                                :at "2026-09-18T00:00:00Z"}}})
    (is (contains? (rules (verdict st {:op :expire-pledge :campaign-id "cf-collecting"
                                       :pledge-id "pl-1" :patch {:at "2026-09-18T00:00:00Z"}}))
                   :premature-expiry)
        "in the final numbers an early drop looks like a backer who never existed")
    (is (= #{} (rules (verdict st {:op :expire-pledge :campaign-id "cf-collecting"
                                   :pledge-id "pl-1" :patch {:at "2026-09-24T00:00:00Z"}}
                              after-window))))))

;; ───────────────────────── the usual invariants ─────────────────────────

(deftest default-mock-advisor-proposals-never-self-trip-scope-exclusion
  (let [st (clean-store)]
    (doseq [req [{:op :plan-collection :campaign-id "cf-collecting"}
                 {:op :record-attempt :campaign-id "cf-collecting" :pledge-id "pl-1"
                  :patch {:outcome :ok}}
                 {:op :retry-pledge :campaign-id "cf-collecting" :pledge-id "pl-1"}
                 {:op :expire-pledge :campaign-id "cf-collecting" :pledge-id "pl-1"}
                 {:op :finalize-collection :campaign-id "cf-collecting"}
                 {:op :flag-collection-concern :campaign-id "cf-collecting"}]]
      (is (not (contains? (rules (verdict st req)) :scope-excluded))
          (str (:op req) " must not block itself")))))

(deftest claiming-to-have-charged-someone-is-permanently-blocked
  (let [st (clean-store)
        p  (advisor/infer st {:op :plan-collection :campaign-id "cf-collecting"
                              :out-of-scope? true})]
    (is (contains? (rules (sut/check {} ctx p st)) :scope-excluded))))

(deftest an-effect-other-than-propose-is-a-claim-to-actuate
  (let [st (clean-store)
        p  (assoc (advisor/infer st {:op :record-attempt :campaign-id "cf-collecting"
                                     :pledge-id "pl-1" :patch {:outcome :ok}})
                  :effect :execute)]
    (is (contains? (rules (sut/check {} ctx p st)) :effect-not-propose))))

(deftest no-op-in-the-allowlist-performs-a-charge
  (is (= #{:plan-collection :record-attempt :retry-pledge :expire-pledge
           :finalize-collection :flag-collection-concern}
         sut/allowed-ops))
  (is (contains? (rules (sut/check {} ctx {:op :charge-card :effect :propose}
                                   (store/seed-db)))
                 :op-not-allowed)))

(deftest the-deciding-ops-are-out-of-every-phase-auto-set
  (doseq [[p {:keys [auto]}] phase/phases
          op sut/always-escalate-ops]
    (is (not (contains? auto op))
        (str op " must never be auto-committable, including phase " p))))

(deftest a-governor-hold-survives-every-phase
  (doseq [p (keys phase/phases)]
    (is (= :hold (:disposition (phase/gate p {:op :record-attempt} :hold))))))

(deftest early-phases-disable-writes-rather-than-quietly-allowing-them
  (is (= {:disposition :hold :reason :phase-disabled}
         (phase/gate 0 {:op :record-attempt} :commit)))
  (is (= {:disposition :escalate :reason :phase-approval}
         (phase/gate 1 {:op :record-attempt} :commit)))
  (is (= {:disposition :hold :reason :phase-disabled}
         (phase/gate 2 {:op :plan-collection} :commit)))
  (is (= {:disposition :commit :reason nil}
         (phase/gate 3 {:op :record-attempt} :commit))))
