module turncoat

open util/ordering[ApiResponse] as oa
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

sig ApiResponse
  { target : one User
  , followers : set User
  , timestamp : one Timestamp
  }
  { all fw : followers | fw.platform = target.platform }

pred sameParticipants[ea, eb : Event] {
  ea.source = eb.source
  ea.target = eb.target
}

pred lastActionWasFollow[src, tgt : User, tm : Timestamp] {
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
  all current : Event | lastActionWasFollow[current.source, current.target, current.timestamp]
    implies current.action = Unfollow else current.action = Follow
}

fact "No duplicate actions with the same timestamp" {
  no disj ea, eb : Event | {
    sameParticipants[ea, eb]
    ea.timestamp = eb.timestamp
    ea.action = eb.action
  }
}

fact "Timestamp order" {
  all ea, eb : Event | oe/lt[ea, eb] iff ot/lt[ea.timestamp, eb.timestamp]
  all ra, rb : ApiResponse | oa/lt[ra, rb] iff ot/lt[ra.timestamp, rb.timestamp]
}

fact "Events are caused by an API response" {
  all e : Event | some r : ApiResponse | e.source in r.followers implies {
    e.target = r.target
    e.timestamp = r.timestamp
    e.action = Follow
  } else {
    e.target = r.target
    e.timestamp = r.timestamp
    e.action = Unfollow
  }
}

fact "New followers create follow events" {
  all r : ApiResponse, u : User |
    (u in r.followers and not lastActionWasFollow[u, r.target, r.timestamp]) implies some e : Event | {
      e.source = u
      e.target = r.target
      e.action = Follow
      e.timestamp = r.timestamp
    }
}

fact "Missing followers create unfollow events" {
  all r : ApiResponse, u : User |
    (u not in r.followers and lastActionWasFollow[u, r.target, r.timestamp]) implies some e : Event | {
      e.source = u
      e.target = r.target
      e.action = Unfollow
      e.timestamp = r.timestamp
    }
}

assert first_action_is_Follow {
  oe/first.action = Follow
}

assert NoNewFollowEventsOnEmptyResponse {
  all r : ApiResponse | no r.followers implies no e : Event | {
    e.action = Follow
    e.timestamp = r.timestamp
  }
}

check first_action_is_Follow
check NoNewFollowEventsOnEmptyResponse

run {} for 3 but exactly 3 User
