(ns collectops.sim
  "Offline demo: watch a still-live campaign refuse to be charged, watch
  one unpriceable pledge refuse the whole plan, charge with a human's
  approval, let a card fail and retry, and finalise with the drop-off
  visible. `clojure -M:dev:run`."
  (:require [collectops.operation :as operation]
            [collectops.store :as store]
            [langgraph.graph :as g]))

(def ^:private ctx {:actor-id "collection-demo" :phase 3 :now "2026-09-17T00:00:00Z"})

(defn- run-req!
  ([actor tid request] (run-req! actor tid request ctx))
  ([actor tid request c] (g/run* actor {:request request :context c} {:thread-id tid})))

(defn -main [& _]
  (let [s (store/seed-db)
        actor (operation/build s)]

    (println "\n=== 1. まだ募集中のキャンペーンは請求できない（all-or-nothing の実体）===")
    (let [r (run-req! actor "sim-1" {:op :plan-collection :campaign-id "cf-live"})]
      (println "  status     :" (:status r))
      (println "  violations :" (mapv :rule (:violations (last (store/ledger s))))))

    (println "\n=== 2. 請求額を確定できない支援が 1 件でもあれば計画ごと拒否 ===")
    (let [r (run-req! actor "sim-2" {:op :plan-collection :campaign-id "cf-collecting"})]
      (println "  status     :" (:status r))
      (println "  violations :" (mapv :rule (:violations (last (store/ledger s)))))
      (println "  未確定      :" (:plan/unresolved (store/plan-for s "cf-collecting"))))

    (println "\n=== 3. 該当支援を失効させてから、請求計画を人間が承認 ===")
    (store/commit-record! s {:op :record-attempt
                             :value {:pledge-id "pl-broken"
                                     :attempt {:outcome :hard-decline
                                               :at "2026-09-17T00:00:00Z"
                                               :reason :unpriceable}}})
    (store/commit-record! s {:op :expire-pledge
                             :value {:campaign-id "cf-collecting" :pledge-id "pl-broken"
                                     :at "2026-09-17T00:00:00Z"}})
    (store/commit-record! s {:op :expire-pledge
                             :value {:campaign-id "cf-collecting" :pledge-id "pl-hard"
                                     :at "2026-09-17T00:00:00Z"}})
    (let [held (run-req! actor "sim-3" {:op :plan-collection :campaign-id "cf-collecting"})]
      (println "  status     :" (:status held) "（承認前）")
      (let [ok (g/run* actor {:approval {:status :approved :by "finance-01"}}
                       {:thread-id "sim-3" :resume? true})
            p  (store/plan s "cf-collecting")]
        (println "  --- 人間 finance-01 が承認 ---")
        (println "  status     :" (:status ok))
        (println "  請求件数    :" (count (:plan/charges p)))
        (println "  請求合計    :" (:plan/total-minor p) (:plan/currency p))
        (println "  承認者      :" (:plan/authorised-by p))
        (println "  預託しない  :" (not (:plan/custodial? p)))))

    (println "\n=== 4. 決済レールからの結果を記録（事実なので自動）===")
    (run-req! actor "sim-4a" {:op :record-attempt :campaign-id "cf-collecting"
                              :pledge-id "pl-1"
                              :patch {:outcome :ok :at "2026-09-18T00:00:00Z"}})
    (run-req! actor "sim-4b" {:op :record-attempt :campaign-id "cf-collecting"
                              :pledge-id "pl-2"
                              :patch {:outcome :ok :at "2026-09-18T00:00:00Z"}})
    (run-req! actor "sim-4c" {:op :record-attempt :campaign-id "cf-collecting"
                              :pledge-id "pl-3"
                              :patch {:outcome :soft-decline :at "2026-09-18T00:00:00Z"
                                      :reason :insufficient-funds}})
    (doseq [id ["pl-1" "pl-2" "pl-3"]]
      (println " " id ":" (:pledge/state (store/pledge-of s id))))

    (println "\n=== 5. 窓が閉じたら失効。早すぎる失効は拒否される ===")
    (let [early (run-req! actor "sim-5a" {:op :expire-pledge :campaign-id "cf-collecting"
                                          :pledge-id "pl-3"
                                          :patch {:at "2026-09-18T00:00:00Z"}})]
      (println "  早すぎる失効:" (:status early)
               (mapv :rule (:violations (last (store/ledger s))))))
    (run-req! actor "sim-5b" {:op :expire-pledge :campaign-id "cf-collecting"
                              :pledge-id "pl-3" :patch {:at "2026-09-24T00:00:00Z"}}
              (assoc ctx :now "2026-09-24T00:00:00Z"))
    (println "  窓を閉じた後:" (:pledge/state (store/pledge-of s "pl-3")))

    (println "\n=== 6. 回収の確定（約束された額と入金額の差が見える）===")
    (let [held (run-req! actor "sim-6" {:op :finalize-collection :campaign-id "cf-collecting"}
                         (assoc ctx :now "2026-09-24T00:00:00Z"))]
      (println "  status     :" (:status held) "（承認前）")
      (let [ok (g/run* actor {:approval {:status :approved :by "finance-01"}}
                       {:thread-id "sim-6" :resume? true})
            f  (store/finalization s "cf-collecting")]
        (println "  --- 人間 finance-01 が承認 ---")
        (println "  status     :" (:status ok))
        (println "  入金        :" (:collection/collected-minor f))
        (println "  失効        :" (:collection/dropped-minor f))
        (println "  脱落率      :" (:collection/drop-off-bps f) "bp")
        (println "  目標到達    :" (:collection/goal-still-met? f)
                 "／不足" (:collection/shortfall-minor f))
        (println "  次状態      :" (:campaign/state (store/campaign-of s "cf-collecting")))))

    (println "\n=== 監査台帳 ===")
    (doseq [f (store/ledger s)]
      (println " " (:t f) (:op f) (or (:basis f) "")))))
