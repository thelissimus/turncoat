module turncoat

open util/ordering[Event] as oe
open util/ordering[Timestamp] as ot

enum Platform { GitHub, Twitter }
enum Action { Follow, Unfollow }

sig Timestamp {}

sig User
  { platform : one Platform
  }

sig Event
  { source : one User
  , target : one User
  , action : one Action
  , timestamp : one Timestamp
  }
  { source != target and source.platform = target.platform }

pred sameParticipants[ea, eb : Event] {
  ea.source = eb.source
  ea.target = eb.target
}

pred followed[src, tgt : User, tm : Timestamp] {
  some e : Event | { // there was some event before
    e.source = src
    e.target = tgt
    e.action = Follow
    ot/lt[e.timestamp, tm]
    no between : Event | { // and no `Unfollow` inbetween
      between.source = src
      between.target = tgt
      between.action = Unfollow
      ot/lt[e.timestamp, between.timestamp] and ot/lt[between.timestamp, tm]
    }
  } // meaning it was the previous event
}

fact "Follow/Unfollow alternation" {
  all e : Event | e.action = Follow implies not followed[e.source, e.target, e.timestamp]
  all e : Event | e.action = Unfollow implies followed[e.source, e.target, e.timestamp]
}

fact "No duplicate actions with the same timestamp" {
  no disj ea, eb : Event | {
    sameParticipants[ea, eb]
    ea.timestamp = eb.timestamp
    ea.action = eb.action
  }
}

fact "Event order matches Timestamp order" {
  all ea, eb : Event | oe/lt[ea, eb] iff ot/lt[ea.timestamp, eb.timestamp]
}

assert first_action_is_Follow {
  oe/first.action = Follow
}

check first_action_is_Follow

run {} for 3 but exactly 3 User
