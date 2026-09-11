# cloud-itonami-crowdfunding-collection

**CollectionAdvisor ⊣ CollectionGovernor** — the actor that authorises
charges against thousands of strangers, and the reason the containment
pattern exists.

A `cloud-itonami` blueprint actor in the workspace's standard shape:
advisor sealed into one node of a [langgraph-clj](https://github.com/kotoba-lang/langgraph)
StateGraph, an independent governor, a 0→3 phase gate, an append-only
audit ledger. Domain rules from
[`kotoba-lang/crowdfunding`](https://github.com/kotoba-lang/crowdfunding);
design record: ADR-2607268500.

## All-or-nothing, in code rather than in the terms of service

A collection plan **refuses to exist** for a campaign that is not
`:funding-succeeded` or `:collecting`. There is no edge from `:live` to
`:collecting` in `crowdfunding.campaign/transitions`, and the governor
re-checks the state independently. A backer on an unfunded campaign
cannot be charged by any sequence of requests this actor accepts.

## The number nobody shows you

"The campaign raised X" and "the creator received X" are different
numbers. Cards expire between pledging and the deadline; issuers decline
charges weeks older than their authorization; backers change banks. On a
large campaign the gap is a double-digit percentage, and a design that
treats collection as one instantaneous event has nowhere to put it.

`finalize` reports both, plus `:collection/drop-off-bps`, plus
`:collection/unresolved` — the pledges that could not be priced at all,
named rather than silently improving the ratio.

## The invariant

> The governor rejects; the actor never writes what it refuses.

Seven HARD checks, un-overridable by any human approval:

| Check | Why |
|---|---|
| Not collectible | a `:live` campaign can never be charged |
| Unresolved charges | one unpriceable pledge refuses the **whole** plan — charging only the ones we can price is how a backer gets billed an amount nobody can reproduce |
| Empty plan | a charge run with nothing in it was built from stale state |
| Unknown attempt outcome | silence is not success |
| Premature expiry | in the final numbers an early drop is indistinguishable from a backer who never existed |
| `:effect` ≠ `:propose` | a claim to actuate outside governance |
| Scope exclusion | claiming to have charged or moved funds |

## Facts auto-commit; decisions do not

- `:record-attempt` writes down what a rail reported. Auto-committable at
  phase 3 — refusing to record a decline does not make the decline go
  away.
- `:plan-collection` decides that thousands of strangers get debited
  today. **Always escalates**, in every phase.
- `:finalize-collection` states what the creator actually received and
  moves the campaign into fulfilment. **Always escalates** — it is the
  number a creator plans production against.

Nothing here moves money. A plan is a computation plus an authorisation;
a rail adapter performs the transfers afterwards, outside this actor.

## Run it

```bash
kbb -M:dev:run     # offline demo: refusals, a human-approved charge run, retries, drop-off
kbb -M:dev:test    # 25 tests
kbb -M:lint
```

## Licence

AGPL-3.0-or-later.
