module turncoat

open util/ordering[Timestamp]

enum Platform { GitHub, Twitter }
enum Action { Follow, Unfollow }

sig Timestamp {}

sig User {
  platform : one Platform,
}

sig Event {
  source : one User,
  target : one User,
  action : one Action,
  timestamp : one Timestamp,
} {
  source != target
  source.platform = target.platform
}

pred sameParticipants[ea, eb : Event] {
  ea.source = eb.source
  ea.target = eb.target
}

pred lastActionWasFollow[src, tgt : User, tm : Timestamp] {
  some e : Event | { // there was some event before
    e.source = src
    e.target = tgt
    e.action = Follow
    lt[e.timestamp, tm]
    no between : Event | { // and no inbetween
      between.source = src
      between.target = tgt
      between.action = Unfollow
      lt[e.timestamp, between.timestamp] and lt[between.timestamp, tm]
    }
  } // meaning it was the previous event
}

fact "Follow/Unfollow alternation" {
  all e : Event | e.action = Follow implies
    not lastActionWasFollow[e.source, e.target, e.timestamp]

  all e : Event | e.action = Unfollow implies
    lastActionWasFollow[e.source, e.target, e.timestamp]
}

fact "No duplicate actions with the same timestamp" {
  no disj ea, eb : Event | {
    sameParticipants[ea, eb]
    ea.timestamp = eb.timestamp
    ea.action = eb.action
  }
}

run {}
