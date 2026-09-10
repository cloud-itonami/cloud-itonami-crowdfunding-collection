(ns collectops.phase
  "Phase 0->3 staged rollout for the collection actor.

    Phase 0  read-only         -- no writes, still governor-gated.
    Phase 1  assisted-record   -- rail outcomes may be recorded, every
                                  write needs human approval.
    Phase 2  assisted-lifecycle -- adds retries and expiries, still
                                  approval-gated.
    Phase 3  supervised auto   -- governor-clean, high-confidence
                                  `:record-attempt`, `:retry-pledge` and
                                  `:expire-pledge` may auto-commit.

  `:plan-collection`, `:finalize-collection` and
  `:flag-collection-concern` are deliberately ABSENT from every phase's
  `:auto` set, INCLUDING phase 3 — a permanent structural fact, not a
  rollout milestone still to come.

  The line is between FACTS and DECISIONS:

    - `:record-attempt` writes down what a rail reported. It is
      auto-committable because it is not a choice; refusing to record a
      decline does not make the decline go away.
    - `:plan-collection` decides that thousands of strangers get debited
      today. Computing it is reversible; authorising it is not.
    - `:finalize-collection` states what the creator actually received and
      moves the campaign into fulfilment. It is the number a creator plans
      production against, and it must have an author.

  Everything auto-committable here records something that already
  happened. Nothing auto-committable causes a charge.
  `collectops.governor`'s own `always-escalate-ops` enforces the same
  invariant independently — two layers, not one, agree on this."
  (:require [collectops.governor :as governor]))

(def read-ops #{})
(def write-ops governor/allowed-ops)

;; NOTE the invariant: the three deciding ops are members of `write-ops`
;; (governor-gated like any write) but are NEVER members of any phase's
;; `:auto` set below. Do not add them there.
(def phases
  {0 {:label "read-only"          :writes #{}                 :auto #{}}
   1 {:label "assisted-record"    :writes #{:record-attempt}  :auto #{}}
   2 {:label "assisted-lifecycle" :writes #{:record-attempt :retry-pledge :expire-pledge}
      :auto #{}}
   3 {:label "supervised-auto"    :writes write-ops
      :auto #{:record-attempt :retry-pledge :expire-pledge}}})

(def default-phase 3)

(defn gate
  "Adjust a governor disposition for the rollout phase. Returns
  {:disposition kw :reason kw|nil}."
  [phase {:keys [op]} governor-disposition]
  (let [{:keys [writes auto]} (get phases phase (get phases default-phase))]
    (cond
      (= :hold governor-disposition)       {:disposition :hold :reason nil}
      (contains? read-ops op)              {:disposition governor-disposition :reason nil}
      (not (contains? writes op))          {:disposition :hold :reason :phase-disabled}
      (and (= :commit governor-disposition)
           (not (contains? auto op)))      {:disposition :escalate :reason :phase-approval}
      :else                                {:disposition governor-disposition :reason nil})))

(defn verdict->disposition
  "Map a CollectionGovernor verdict to a base disposition before the phase
  gate."
  [verdict]
  (cond (:hard? verdict) :hold
        (:escalate? verdict) :escalate
        :else :commit))
