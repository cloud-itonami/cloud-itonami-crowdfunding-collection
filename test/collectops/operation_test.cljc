(ns collectops.operation-test
  (:require [clojure.test :refer [deftest is testing]]
            [collectops.operation :as operation]
            [collectops.store :as store]
            [langgraph.graph :as g]))

(def ctx {:actor-id "collection-test" :phase 3 :now "2026-09-17T00:00:00Z"})

(defn- run-req!
  ([actor tid request] (run-req! actor tid request ctx))
  ([actor tid request c] (g/run* actor {:request request :context c} {:thread-id tid})))

(defn- ready
  "Seed with the two uncollectable pledges expired, so a plan is
  well-formed."
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

(deftest a-charge-run-interrupts-and-only-a-named-human-authorises-it
  (let [s (ready)
        a (operation/build s)
        held (run-req! a "t1" {:op :plan-collection :campaign-id "cf-collecting"})]
    (is (nil? (store/plan s "cf-collecting")) "nothing authorised before approval")
    (is (some #(= :approval-requested (:t %)) (:audit (:state held))))
    (let [ok (g/run* a {:approval {:status :approved :by "finance-01"}}
                     {:thread-id "t1" :resume? true})
          p  (store/plan s "cf-collecting")]
      (is (= :commit (:disposition (:state ok))))
      (is (= 3 (count (:plan/charges p))) "pl-1, pl-2, pl-3")
      (is (= 76800 (:plan/total-minor p)) "3 x (24800 goods + 800 shipping)")
      (is (= "finance-01" (:plan/authorised-by p)))
      (is (false? (:plan/custodial? p)) "authorising is not transferring"))))

(deftest a-still-live-campaign-never-reaches-the-interrupt
  (let [s (ready)
        a (operation/build s)
        r (run-req! a "t2" {:op :plan-collection :campaign-id "cf-live"})]
    (is (= :hold (:disposition (:state r))))
    (is (not-any? #(= :approval-requested (:t %)) (:audit (:state r)))
        "a human is not asked whether to break all-or-nothing")
    (is (contains? (set (:basis (last (store/ledger s)))) :not-collectible))))

(deftest recording-rail-outcomes-auto-commits-and-drives-the-state-table
  (let [s (ready)
        a (operation/build s)]
    (run-req! a "t3a" {:op :record-attempt :campaign-id "cf-collecting" :pledge-id "pl-1"
                       :patch {:outcome :ok :at "2026-09-18T00:00:00Z"}})
    (run-req! a "t3b" {:op :record-attempt :campaign-id "cf-collecting" :pledge-id "pl-3"
                       :patch {:outcome :soft-decline :at "2026-09-18T00:00:00Z"
                               :reason :insufficient-funds}})
    (is (= :collected (:pledge/state (store/pledge-of s "pl-1"))))
    (is (= :failed (:pledge/state (store/pledge-of s "pl-3"))))
    (is (true? (:pledge/retryable? (store/pledge-of s "pl-3"))))))

(deftest a-retry-then-a-success-collects
  (let [s (ready)
        a (operation/build s)]
    (run-req! a "t4a" {:op :record-attempt :campaign-id "cf-collecting" :pledge-id "pl-2"
                       :patch {:outcome :soft-decline :at "2026-09-18T00:00:00Z"}})
    (run-req! a "t4b" {:op :retry-pledge :campaign-id "cf-collecting" :pledge-id "pl-2"
                       :patch {:at "2026-09-19T00:00:00Z"}})
    (is (= :retrying (:pledge/state (store/pledge-of s "pl-2"))))
    (run-req! a "t4c" {:op :record-attempt :campaign-id "cf-collecting" :pledge-id "pl-2"
                       :patch {:outcome :ok :at "2026-09-19T00:00:00Z"}})
    (is (= :collected (:pledge/state (store/pledge-of s "pl-2"))))
    (is (= 2 (:pledge/attempts (store/pledge-of s "pl-2"))))))

(deftest finalisation-interrupts-and-reports-the-gap-between-promised-and-received
  (let [s (ready)
        a (operation/build s)
        late (assoc ctx :now "2026-09-24T00:00:00Z")]
    (doseq [id ["pl-1" "pl-2"]]
      (run-req! a (str "t5-" id) {:op :record-attempt :campaign-id "cf-collecting"
                                  :pledge-id id
                                  :patch {:outcome :ok :at "2026-09-18T00:00:00Z"}}))
    (run-req! a "t5-3" {:op :record-attempt :campaign-id "cf-collecting" :pledge-id "pl-3"
                        :patch {:outcome :soft-decline :at "2026-09-18T00:00:00Z"}})
    (run-req! a "t5-x" {:op :expire-pledge :campaign-id "cf-collecting" :pledge-id "pl-3"
                        :patch {:at "2026-09-24T00:00:00Z"}} late)
    (is (= :dropped (:pledge/state (store/pledge-of s "pl-3"))))
    (run-req! a "t5" {:op :finalize-collection :campaign-id "cf-collecting"} late)
    (is (= :collecting (:campaign/state (store/campaign-of s "cf-collecting")))
        "nothing moved before approval")
    (let [ok (g/run* a {:approval {:status :approved :by "finance-01"}}
                     {:thread-id "t5" :resume? true})
          f  (store/finalization s "cf-collecting")]
      (is (= :commit (:disposition (:state ok))))
      (is (= 51200 (:collection/collected-minor f)) "two of five actually paid")
      (is (= 51200 (:collection/dropped-minor f))
          "pl-3 and pl-hard dropped; pl-broken cannot be priced and so cannot be summed")
      (is (= ["pl-broken"] (:collection/unresolved f))
          "and it is named rather than quietly improving the ratio")
      (is (= 5000 (:collection/drop-off-bps f)) "50% drop-off, stated rather than hidden")
      (is (false? (:collection/goal-still-met? f)))
      (is (= 450400 (:collection/shortfall-minor f)))
      (is (= :fulfilling (:campaign/state (store/campaign-of s "cf-collecting")))
          "backers were charged, so the obligation stands")
      (is (= "finance-01" (:collection/authorised-by f))))))

(deftest a-rejected-approval-holds-and-authorises-nothing
  (let [s (ready)
        a (operation/build s)]
    (run-req! a "t6" {:op :plan-collection :campaign-id "cf-collecting"})
    (let [no (g/run* a {:approval {:status :rejected :by "finance-01"}}
                     {:thread-id "t6" :resume? true})]
      (is (= :hold (:disposition (:state no))))
      (is (nil? (store/plan s "cf-collecting")))
      (is (some #(= :approval-rejected (:t %)) (store/ledger s))))))

(deftest phase-two-cannot-authorise-a-charge-run
  (testing "the rollout gate and the governor agree independently"
    (let [s (ready)
          a (operation/build s)
          r (run-req! a "t7" {:op :plan-collection :campaign-id "cf-collecting"}
                      (assoc ctx :phase 2))]
      (is (= :hold (:disposition (:state r))))
      (is (nil? (store/plan s "cf-collecting"))))))
