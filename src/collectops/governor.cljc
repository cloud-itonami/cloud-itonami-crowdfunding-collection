(ns collectops.governor
  "CollectionGovernor — the independent compliance layer standing between
  a computed collection and the moment backers' cards are charged.

  This is the actor where the containment pattern earns its keep. Every
  other crowdfunding actor manipulates records; this one authorises debits
  against thousands of strangers, weeks after they agreed to them, on the
  strength of a state transition nobody re-checked. So this MUST be a
  separate system able to *reject* a proposal and fall back to HOLD.

  ## Nothing here moves money

  A collection plan is a COMPUTATION plus an AUTHORISATION. A rail adapter
  performs the transfers after a human approves the plan, outside this
  actor. Any claim to have *already* charged, captured or moved funds is a
  HARD, permanent scope exclusion.

  Seven HARD checks, ALL permanent, un-overridable by any human approval:

    1. Not collectible          -- the campaign must be `:funding-succeeded`
                                   or `:collecting`. A `:live` campaign can
                                   never be charged. This is what
                                   all-or-nothing MEANS in code, rather than
                                   in the terms of service.
    2. Unresolved charges       -- a plan containing a pledge whose total
                                   cannot be computed is refused whole. The
                                   alternative — charge the ones we can
                                   price — is how a backer gets billed an
                                   amount nobody could reproduce.
    3. Empty plan               -- authorising a charge run with nothing in
                                   it is a request built from stale state.
    4. Unknown attempt outcome  -- an outcome that is neither `:ok` nor a
                                   named decline must not be interpreted as
                                   either. Silence is not success.
    5. Premature expiry         -- dropping a pledge that is still
                                   retryable. In the final numbers an early
                                   drop is indistinguishable from a backer
                                   who never existed.
    6. Effect not :propose      -- any other value is a claim to directly
                                   actuate outside governance.
    7. Scope exclusion          -- any claim to have charged or moved funds,
                                   plus any op outside the closed allowlist.

  Two ESCALATE (SOFT) gates:
    - LLM confidence below the floor.
    - `:plan-collection`, `:finalize-collection` and
      `:flag-collection-concern` ALWAYS escalate. Planning is the moment
      money leaves; finalising is the statement of what the creator
      actually received and moves the campaign into fulfilment.
      `collectops.phase` keeps all three out of every phase's `:auto` set
      independently — two layers, not one."
  (:require [clojure.string :as str]
            [collectops.store :as store]
            [crowdfunding.collection :as collection]))

(def confidence-floor 0.6)

(def allowed-ops
  "The closed proposal-op allowlist. CRITICAL: no op that performs a
  charge is EVER a member — such an op would be a permanent scope
  violation, not merely un-implemented. `:plan-collection` AUTHORISES
  charges for a rail to perform; it does not perform any."
  #{:plan-collection :record-attempt :retry-pledge :expire-pledge
    :finalize-collection :flag-collection-concern})

(def always-escalate-ops
  #{:plan-collection :finalize-collection :flag-collection-concern})

(def scope-excluded-terms
  "Case-insensitive substrings marking a proposal as claiming to have
  moved money.

  CRITICAL: every term is phrased as the COMPLETED act ('charged the
  backers'), never a bare noun like 'charge' or 'collection' — a bare noun
  would match inside this actor's own legitimate proposals (whose whole
  job is to talk about collecting charges) and self-block the happy path.
  See
  `collectops.governor-test/default-mock-advisor-proposals-never-self-trip-scope-exclusion`."
  ["charged the backers" "charged the backer" "have charged"
   "captured the payments" "captured the funds" "took the payments"
   "moved the funds" "transferred the funds" "have transferred"
   "disbursed the funds" "paid out the creator" "have paid out"
   "refunded the backers" "have refunded" "resolved the payment dispute"
   "支援者に請求した" "引き落とした" "決済を実行した" "決済を完了した"
   "資金を移動した" "送金した" "返金した" "支払い紛争を解決した"])

;; ----------------------------- checks -----------------------------

(defn- plan-of
  "The plan a proposal is about: the one it carries, or the one this store
  computes for the campaign it names. Re-derived rather than trusted."
  [proposal st]
  (or (get-in proposal [:value :plan])
      (some->> (get-in proposal [:value :campaign-id]) (store/plan-for st))))

(defn- plan-violations
  "Applies to `:plan-collection`. Every branch here is a way a charge run
  can be wrong in a manner nobody notices until statements arrive."
  [proposal st]
  (when (= :plan-collection (:op proposal))
    (let [p (plan-of proposal st)]
      (cond
        (nil? p)
        [{:rule :plan-missing :detail "対象の請求計画が特定できない"}]

        (:plan/refused p)
        [{:rule :not-collectible
          :detail (str "状態 " (pr-str (:plan/detail p))
                       " では請求できない（all-or-nothing はここで効く）")}]

        (seq (:plan/unresolved p))
        [{:rule :unresolved-charges
          :detail (str "請求額を確定できない支援がある: "
                       (str/join "," (:plan/unresolved p))
                       " — 一部だけ請求するのではなく計画ごと拒否する")}]

        (empty? (:plan/charges p))
        [{:rule :empty-plan :detail "請求対象が 0 件の請求計画は承認しない"}]))))

(defn- attempt-violations
  "An outcome that is neither `:ok` nor a named decline must not be
  interpreted as either. Silence is not success."
  [proposal st]
  (when (= :record-attempt (:op proposal))
    (let [outcome (get-in proposal [:value :attempt :outcome])
          id      (get-in proposal [:value :pledge-id])
          p       (store/pledge-of st id)]
      (cond
        (nil? p)
        [{:rule :pledge-unknown :detail (str (or id "(id missing)") " は存在しない")}]

        (not (contains? collection/attempt-outcomes outcome))
        [{:rule :unknown-attempt-outcome
          :detail (str (pr-str outcome) " は :ok / :soft-decline / :hard-decline のいずれでもない")}]

        (not (contains? #{:authorized :retrying} (:pledge/state p)))
        [{:rule :pledge-not-chargeable
          :detail (str "状態 " (pr-str (:pledge/state p)) " の支援に決済結果は適用できない")}]))))

(defn- retry-violations
  "A retry is bounded in attempts AND in time, and a hard decline is never
  retried — retrying one is not persistence, it is a fraud signal to the
  issuer."
  [proposal st now]
  (when (= :retry-pledge (:op proposal))
    (let [id (get-in proposal [:value :pledge-id])
          p  (store/pledge-of st id)
          w  (store/window st (get-in proposal [:value :campaign-id]))]
      (cond
        (nil? p) [{:rule :pledge-unknown :detail (str (or id "(id missing)") " は存在しない")}]
        (nil? w) [{:rule :window-unknown :detail "回収窓が設定されていない"}]
        (not (collection/retryable? p w now))
        [{:rule :not-retryable
          :detail (str "retryable? " (:pledge/retryable? p) " / attempts "
                       (:pledge/attempts p) " / window "
                       (if (collection/window-closed? w now) "closed" "open"))}]))))

(defn- expiry-violations
  "Dropping a pledge that is still retryable."
  [proposal st now]
  (when (= :expire-pledge (:op proposal))
    (let [id (get-in proposal [:value :pledge-id])
          p  (store/pledge-of st id)
          w  (store/window st (get-in proposal [:value :campaign-id]))]
      (cond
        (nil? p) [{:rule :pledge-unknown :detail (str (or id "(id missing)") " は存在しない")}]
        (nil? w) [{:rule :window-unknown :detail "回収窓が設定されていない"}]
        (nil? (collection/expire p w now))
        [{:rule :premature-expiry
          :detail "まだ再試行可能・または対象状態でない支援は失効させない（早すぎる失効は総額を過小に見せる）"}]))))

(defn- effect-not-propose-violations [proposal]
  (when (not= :propose (:effect proposal))
    [{:rule :effect-not-propose
      :detail (str ":effect は :propose のみ許可されるが " (pr-str (:effect proposal)) " が提案された")}]))

(defn- text-blob [proposal]
  (str/lower-case (pr-str (select-keys proposal [:op :summary :rationale :cites]))))

(defn- scope-exclusion-violations [proposal]
  (let [op (:op proposal)
        blob (text-blob proposal)]
    (cond
      (not (contains? allowed-ops op))
      [{:rule :op-not-allowed
        :detail (str (pr-str op) " は許可された操作(closed allowlist)に含まれない")}]

      (some #(str/includes? blob %) scope-excluded-terms)
      [{:rule :scope-excluded
        :detail "請求・送金・返金・紛争解決を実行済みと主張する提案は永久に禁止"}])))

(defn check
  "Censors a CollectionAdvisor proposal. `context` supplies `:now`
  (ISO-8601 UTC) — this governor has no clock of its own.

  Returns {:ok? bool :violations [..] :confidence c :escalate? bool
           :high-stakes? bool :hard? bool}."
  [_request context proposal store]
  (let [now  (:now context)
        hard (into []
                   (concat (plan-violations proposal store)
                           (attempt-violations proposal store)
                           (retry-violations proposal store now)
                           (expiry-violations proposal store now)
                           (effect-not-propose-violations proposal)
                           (scope-exclusion-violations proposal)))
        conf (:confidence proposal 0.0)
        low? (< conf confidence-floor)
        stakes? (boolean (always-escalate-ops (:op proposal)))
        hard? (boolean (seq hard))]
    {:ok?          (and (not hard?) (not low?) (not stakes?))
     :violations   hard
     :confidence   conf
     :hard?        hard?
     :escalate?    (and (not hard?) (or low? stakes?))
     :high-stakes? stakes?}))

(defn hold-fact [request context verdict]
  {:t           :governor-hold
   :op          (:op request)
   :actor       (:actor-id context)
   :campaign-id (:campaign-id request)
   :pledge-id   (:pledge-id request)
   :disposition :hold
   :basis       (mapv :rule (:violations verdict))
   :violations  (:violations verdict)
   :confidence  (:confidence verdict)})
