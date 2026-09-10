(ns collectops.advisor
  "CollectionAdvisor — the *contained intelligence node* for the
  collection actor.

  It drafts exactly six kinds of proposal from a closed allowlist:
  planning a collection, recording an attempt outcome, retrying a failed
  pledge, expiring an uncollectable one, finalising the window, and
  flagging a concern.

  CRITICAL: it is a smart-but-untrusted advisor, and this is the actor
  where that matters most, because this is the moment backers are actually
  charged. Every proposal's `:effect` is always `:propose`; nothing here
  moves funds. Every output is censored downstream by
  `collectops.governor`.

  Note what the advisor does NOT decide. WHO is charged and HOW MUCH comes
  from `crowdfunding.collection/collection-plan` over the store's own
  pledge and reward records, and the governor independently re-derives the
  same plan. An advisor cannot charge a backer more by being persuasive —
  it can only propose that the arithmetic be run and the result be shown
  to a human.

  Like every sibling actor's advisor this is a deterministic mock so the
  actor graph runs offline. In production this calls a real LLM with the
  same proposal shape."
  (:require [collectops.store :as store]))

(defprotocol Advisor
  (-advise [advisor store request] "store + request -> proposal map"))

(defn- propose-plan
  "Draft a collection plan. ALWAYS escalates — this is the moment money
  leaves backers' cards. The rationale describes the AUTHORISATION, never
  a completed charge, so it never trips `scope-excluded-terms`."
  [st {:keys [campaign-id]}]
  (let [p (store/plan-for st campaign-id)]
    {:op          :plan-collection
     :campaign-id campaign-id
     :summary     (if (:plan/refused p)
                    (str campaign-id " の請求計画は作成できない: " (pr-str (:plan/refused p)))
                    (str campaign-id " の請求計画: " (count (:plan/charges p)) " 件 / 合計 "
                         (:plan/total-minor p) " " (:plan/currency p)))
     :rationale   "確定した結果に基づく請求の承認依頼のみ。実際の引き落としは承認後に決済レールが行う。"
     :cites       [campaign-id]
     :effect      :propose
     :value       {:campaign-id campaign-id :plan p}
     :confidence  0.93}))

(defn- propose-attempt
  "Record what the rail reported. This is a FACT arriving from outside,
  not a decision — which is why it is auto-eligible while planning is
  not."
  [_st {:keys [campaign-id pledge-id patch]}]
  {:op          :record-attempt
   :campaign-id campaign-id
   :pledge-id   pledge-id
   :summary     (str pledge-id " の決済試行結果を記録: " (pr-str (:outcome patch)))
   :rationale   "決済レールから返った結果事実の記録のみ。判断は含まない。"
   :cites       [pledge-id]
   :effect      :propose
   :value       {:campaign-id campaign-id :pledge-id pledge-id
                 :attempt {:outcome (:outcome patch) :at (:at patch)
                           :reason (:reason patch)}}
   :confidence  0.95})

(defn- propose-retry
  [_st {:keys [campaign-id pledge-id patch]}]
  {:op          :retry-pledge
   :campaign-id campaign-id
   :pledge-id   pledge-id
   :summary     (str pledge-id " の再試行を提案")
   :rationale   "窓と試行回数の範囲内での再試行の記録のみ。ハード拒否は再試行しない。"
   :cites       [pledge-id]
   :effect      :propose
   :value       {:campaign-id campaign-id :pledge-id pledge-id :at (:at patch)}
   :confidence  0.9})

(defn- propose-expire
  [_st {:keys [campaign-id pledge-id patch]}]
  {:op          :expire-pledge
   :campaign-id campaign-id
   :pledge-id   pledge-id
   :summary     (str pledge-id " の失効を提案")
   :rationale   "回収不能となった支援の失効記録のみ。早すぎる失効は総額を過小に見せる。"
   :cites       [pledge-id]
   :effect      :propose
   :value       {:campaign-id campaign-id :pledge-id pledge-id :at (:at patch)}
   :confidence  0.88})

(defn- propose-finalize
  "Draft the finalisation. ALWAYS escalates — it states what the creator
  actually received, and moves the campaign into fulfilment."
  [st {:keys [campaign-id]}]
  (let [f (store/finalize-for st campaign-id)]
    {:op          :finalize-collection
     :campaign-id campaign-id
     :summary     (str campaign-id " の回収確定: 入金 " (:collection/collected-minor f)
                       " / 失効 " (:collection/dropped-minor f)
                       " (脱落 " (:collection/drop-off-bps f) "bp)")
     :rationale   "回収結果の確定と次状態の提案のみ。承認は人間が行う。"
     :cites       [campaign-id]
     :effect      :propose
     :value       {:campaign-id campaign-id :finalization f}
     :confidence  0.92}))

(defn- propose-concern
  [_st {:keys [campaign-id pledge-id patch]}]
  {:op          :flag-collection-concern
   :campaign-id campaign-id
   :pledge-id   pledge-id
   :summary     (str (or pledge-id campaign-id) " に関する回収上の懸念フラグ: "
                     (pr-str (:concern patch "unknown")))
   :rationale   "観察された懸念事実の報告のみ。請求・返金・紛争の裁定は行わない。"
   :cites       (vec (keep identity [campaign-id pledge-id]))
   :effect      :propose
   :value       (merge {:campaign-id campaign-id :pledge-id pledge-id} patch)
   :confidence  (or (:confidence patch) 0.78)})

(defn infer
  [st {:keys [op out-of-scope?] :as request}]
  (let [proposal (case op
                   :plan-collection         (propose-plan st request)
                   :record-attempt          (propose-attempt st request)
                   :retry-pledge            (propose-retry st request)
                   :expire-pledge           (propose-expire st request)
                   :finalize-collection     (propose-finalize st request)
                   :flag-collection-concern (propose-concern st request)
                   {})]
    ;; Test hook: inject scope-excluded content to exercise the
    ;; governor's scope-exclusion block end-to-end. Clear before
    ;; production use.
    (if out-of-scope?
      (update proposal :rationale str
              " -- actually charged the backers and moved the funds")
      proposal)))

(defn trace [_request proposal]
  {:t           :advisor-proposal
   :op          (:op proposal)
   :campaign-id (:campaign-id proposal)
   :pledge-id   (:pledge-id proposal)
   :summary     (:summary proposal)
   :confidence  (:confidence proposal)})

(defn mock-advisor []
  (reify Advisor
    (-advise [_ store request]
      (infer store request))))
