module turncoat

open util/ordering[Id]
open util/ordering[Timestamp]

enum Platform { GitHub, Twitter }
enum Action { Follow, Unfollow }

sig Id {}
sig Username {}
sig Timestamp {}

sig User {
  platform : one Platform,
  platformId : one Id,
  platformUsername : one Username,
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

fact "Unique usernames and identifiers within platform" {
  all disj ua, ub : User | ua.platform = ub.platform implies {
    ua.platformId != ub.platformId
    ua.platformUsername != ub.platformUsername
  }
}

check NoDuplicateUsernamesOrIds {
  no disj ca, cb : User | ca.platform = cb.platform and
    (ca.platformId = cb.platformId or ca.platformUsername = cb.platformUsername)
}

pred sameParticipants[ea, eb : Event] {
  ea.source = eb.source
  ea.target = eb.target
}

fact "Follow/Unfollow alternation" {
  all e : Event | e.action = Follow implies {
    no eb : Event | {
      sameParticipants[eb, e]
      eb.action = Follow
      lt[eb.timestamp, e.timestamp]
      no between : Event | {
        sameParticipants[between, e]
        between.action = Unfollow
        lt[eb.timestamp, between.timestamp]
        lt[between.timestamp, e.timestamp]
      }
    }
  }

  all e : Event | e.action = Unfollow implies {
    some eb : Event | {
      sameParticipants[eb, e]
      eb.action = Follow
      lt[eb.timestamp, e.timestamp]
      no between : Event | {
        sameParticipants[between, e]
        lt[eb.timestamp, between.timestamp]
        lt[between.timestamp, e.timestamp]
      }
    }
  }
}

fact "Unique timestamps" {
  no disj ea, eb : Event | ea.timestamp = eb.timestamp
}

run {}
