(ns collectops.store
  "SSoT for the collection actor — what was authorised, what was
  attempted, and what actually landed.

  Directories, all keyed by STRING ids (never keywords):

    campaigns  campaign id -> campaign record. Read for its state; a
               charge is only legitimate from a DECIDED outcome, and that
               is read here rather than asserted by a request.
    rewards    campaign id -> reward tiers, needed to price a charge.
    pledges    pledge id -> pledge record, including its attempt count and
               decline reason.
    windows    campaign id -> the collection window (open/close, retry
               budget).

  This store holds no money and no keys. It records that a charge was
  AUTHORISED and what a rail reported back; the rail performs the transfer
  outside this actor, after a human approves the plan. Nothing here can
  move funds, and no phase makes that possible.

  The ledger stays append-only."
  (:require [crowdfunding.campaign :as cf]
            [crowdfunding.collection :as collection]
            [crowdfunding.pledge :as pledge]
            [crowdfunding.reward :as reward]))

(defprotocol Store
  (campaign-of [s id] "Campaign record, or nil.")
  (rewards [s campaign-id])
  (pledge-of [s id])
  (pledges-for [s campaign-id])
  (window [s campaign-id] "The collection window for a campaign, or nil.")
  (ledger [s])
  (collection-log [s])
  (commit-record! [s record])
  (append-ledger! [s fact]))

;; ----------------------------- demo data -----------------------------

(def ^:private risks
  (str "Tooling is not finalised and the moulder quoted six weeks that "
       "could slip. The controller is single-source."))

(defn- a-campaign [id state goal]
  (assoc (cf/campaign {:id id :creator "creator.alpha" :title (str id)
                       :currency "JPY" :goal-minor goal :category :technology
                       :duration-days 45 :launched-at "2026-08-01T00:00:00Z"
                       :deadline "2026-09-15T00:00:00Z"
                       :story "A split keyboard." :risks risks
                       :ships-to #{:jp}})
         :campaign/state state))

(defn- a-pledge [id campaign backer & [overrides]]
  (merge (pledge/pledge {:id id :campaign campaign :backer backer
                         :amount-minor 24800 :reward "standard" :ship-to :jp
                         :placed-at "2026-08-10T00:00:00Z"})
         overrides))

(defn demo-data
  "Fixtures covering the happy path and each hard check.

    cf-collecting  :collecting, three authorised pledges -> chargeable
    cf-live        still :live                           -> refuses to charge
                   (this is what all-or-nothing MEANS in code)

    pl-broken      an authorised pledge whose add-on does not resolve, so
                   its total is uncomputable and it must never be charged
                   at a guessed amount
    pl-hard        already hard-declined, so a retry is refused"
  []
  {:campaigns {"cf-collecting" (a-campaign "cf-collecting" :collecting 500000)
               "cf-live"       (a-campaign "cf-live" :live 500000)}
   :rewards
   (let [tiers [(reward/reward {:id "standard" :title "Standard" :minimum-minor 24800
                                :estimated-delivery "2027-04-01"
                                :shipping {:jp 800} :ships-to #{:jp}})]]
     {"cf-collecting" tiers "cf-live" tiers})
   :pledges
   {"pl-1"      (a-pledge "pl-1" "cf-collecting" "backer.one")
    "pl-2"      (a-pledge "pl-2" "cf-collecting" "backer.two")
    "pl-3"      (a-pledge "pl-3" "cf-collecting" "backer.three")
    "pl-broken" (a-pledge "pl-broken" "cf-collecting" "backer.four"
                          {:pledge/add-ons [{:add-on/reward "ghost" :add-on/qty 1}]})
    "pl-hard"   (a-pledge "pl-hard" "cf-collecting" "backer.five"
                          {:pledge/state :failed :pledge/retryable? false
                           :pledge/attempts 1 :pledge/decline-reason :account-closed})
    "pl-live"   (a-pledge "pl-live" "cf-live" "backer.six")}
   :windows
   {"cf-collecting" (collection/window {:opened-at "2026-09-16T00:00:00Z"
                                        :closes-at "2026-09-23T00:00:00Z"})}})

;; ----------------------------- MemStore -----------------------------

(defrecord MemStore [a]
  Store
  (campaign-of [_ id] (get-in @a [:campaigns id]))
  (rewards [_ id] (get-in @a [:rewards id] []))
  (pledge-of [_ id] (get-in @a [:pledges id]))
  (pledges-for [_ cid] (->> (vals (:pledges @a))
                            (filter #(= cid (:pledge/campaign %)))
                            (sort-by :pledge/id)
                            vec))
  (window [_ cid] (get-in @a [:windows cid]))
  (ledger [_] (:ledger @a))
  (collection-log [_] (:collection-log @a))
  (commit-record! [_ record]
    (swap! a update :collection-log conj record)
    (let [{:keys [op value payload]} record
          id (:pledge-id value)]
      (case op
        ;; A plan is an AUTHORISATION to charge, recorded with the name of
        ;; the human who authorised it. It moves nothing; a rail reads the
        ;; authorised plan and performs the transfers outside this actor.
        :plan-collection
        (swap! a assoc-in [:plans (:campaign-id value)]
               (assoc (:plan value) :plan/authorised-by (:approved-by payload)))

        ;; An attempt outcome is a FACT the rail reported. Applying it
        ;; through `crowdfunding.collection/record-attempt` rather than by
        ;; setting a state directly is what keeps the pledge state table
        ;; the only description of what can follow what.
        :record-attempt
        (swap! a update-in [:pledges id]
               #(or (collection/record-attempt % (:attempt value)) %))

        :retry-pledge
        (swap! a update-in [:pledges id]
               (fn [p] (or (collection/retry p (get-in @a [:windows (:campaign-id value)])
                                             (:at value))
                           p)))

        :expire-pledge
        (swap! a update-in [:pledges id]
               (fn [p] (or (collection/expire p (get-in @a [:windows (:campaign-id value)])
                                              (:at value))
                           p)))

        :finalize-collection
        (swap! a
               (fn [m]
                 (-> m
                     (assoc-in [:finalizations (:campaign-id value)]
                               (assoc (:finalization value)
                                      :collection/authorised-by (:approved-by payload)))
                     (update-in [:campaigns (:campaign-id value)]
                                #(when % (or (collection/apply-finalization
                                              % (:finalization value))
                                             %))))))
        nil))
    record)
  (append-ledger! [_ fact] (swap! a update :ledger conj fact) fact))

(defn seed-db []
  (->MemStore (atom (assoc (demo-data) :ledger [] :collection-log []
                           :plans {} :finalizations {}))))

(defn mem-store [m]
  (->MemStore (atom (merge {:campaigns {} :rewards {} :pledges {} :windows {}
                            :plans {} :finalizations {}
                            :ledger [] :collection-log []}
                           m))))

;; ----------------------------- derived views -----------------------------

(defn plan-for
  "Compute (not commit) the collection plan for a campaign, from THIS
  store's campaign state, pledges and reward prices."
  [s cid]
  (collection/collection-plan (campaign-of s cid) (pledges-for s cid) (rewards s cid)))

(defn finalize-for
  "Compute (not commit) the finalisation for a campaign."
  [s cid]
  (collection/finalize (campaign-of s cid) (pledges-for s cid) (rewards s cid)))

(defn plan [s cid] (get-in @(:a s) [:plans cid]))
(defn finalization [s cid] (get-in @(:a s) [:finalizations cid]))
