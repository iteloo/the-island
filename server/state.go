package main

import (
	"fmt"
	"log"
	"math/rand"
	"time"
)

type GameState string

const (
	WaitingState       GameState = "waiting"
	SiteSelectionState GameState = "site_selection"
	SiteVisitState     GameState = "site_visit"
)

const (
	// MinPlayers sets the minimum number of players required before the game
	// will proceed past the Waiting stage.
	MinPlayers int = 1

	MaxEventsPerRound int = 3

	// Amount of time to wait while site is selected.
	SiteSelectionDuration time.Duration = 2 * time.Second
	// Number of rounds during the site visit.
	NumSiteVisitRounds int = 5
	// Length of each round
	SiteVisitRoundDuration time.Duration = 2 * time.Second
	// Time allocated for status updates, if any
	SiteVisitStatusDuration time.Duration = 2 * time.Second
)

type StateController interface {
	Name() GameState
	Begin()
	End()
	Timer(tick time.Duration)
	RecieveMessage(User, Message)
}

type WaitingController struct {
	game  *Game
	name  GameState
	ready map[User]bool
}

func NewWaitingController(game *Game) *WaitingController {
	return &WaitingController{
		game:  game,
		name:  WaitingState,
		ready: map[User]bool{},
	}
}

// Name returns the name of the current state.
func (s *WaitingController) Name() GameState { return s.name }

// Begin is called when the state becomes active.
func (s *WaitingController) Begin() {}

// End is called when the state is no longer active.
func (s *WaitingController) End() {}

// Timer is called when a timeout occurs.
func (s *WaitingController) Timer(tick time.Duration) {}

// RecieveMessage is called when a user sends a message to the server.
func (s *WaitingController) RecieveMessage(u User, m Message) {
	log.Printf("Ready state: %v", s.ready)
	switch msg := m.(type) {
	case ReadyMessage:
		s.ready[u] = msg.Ready
	case JoinMessage:
		s.ready[u] = false
	case LeaveMessage:
		delete(s.ready, u)
	case SetNameMessage:
		// Just send a playerinfo update (done below),
		// no need to take action, since
		// this is done by the game controller.
	default:
		return
	}

	// Inform all of the clients of the ready state of the other
	// clients.
	var info []PlayerInfo
	for u, ready := range s.ready {
		info = append(info, PlayerInfo{
			Name:  u.Name(),
			Ready: ready,
		})
	}
	s.game.connection.Broadcast(NewPlayerInfoUpdateMessage(info))
	s.proceedIfReady()
}

func (s *WaitingController) proceedIfReady() {
	count := 0
	for _, v := range s.ready {
		if !v {
			return
		}
		count++
	}

	if count >= s.game.MinPlayers {
		s.game.ChangeState(SiteSelectionState)
	}
}

type SiteSelectionController struct {
	game *Game
	name GameState
}

func NewSiteSelectionController(game *Game) *SiteSelectionController {
	// Reset all selected sites.
	for u, _ := range game.UserSites {
		game.UserSites[u] = NoSiteSelected
	}

	return &SiteSelectionController{
		game: game,
		name: WaitingState,
	}
}

// Name returns the name of the current state.
func (s *SiteSelectionController) Name() GameState { return s.name }

// Begin is called when the state becomes active.
func (s *SiteSelectionController) Begin() {}

// End is called when the state is no longer active.
func (s *SiteSelectionController) End() {}

// Timer is called when a timeout occurs.
func (s *SiteSelectionController) Timer(tick time.Duration) {}

// RecieveMessage is called when a user sends a message to the server.
func (s *SiteSelectionController) RecieveMessage(u User, m Message) {
	switch msg := m.(type) {
	case SiteSelectionMessage:
		s.game.UserSites[u] = msg.SiteSelected
	default:
		return
	}

	// Since a site was selected, check if everyone picked a site. If so, we can proceed.
	ready := true
	for u, s := range s.game.UserSites {
		if s == NoSiteSelected {
			ready = false
		}
		fmt.Printf("user %q chose %q", u.Name(), s)
	}

	if ready {
		s.game.ChangeState(SiteVisitState)
	}
}

type SiteVisitController struct {
	game *Game
	name GameState

	userEventQueue      map[User][]SiteEvent
	nextMessageID       uint64
	messageHandlers     map[uint64]SiteEvent
	eventFinishHandlers map[User]uint64

	statusPhase bool
}

func NewSiteVisitController(game *Game) *SiteVisitController {
	return &SiteVisitController{
		game:                game,
		name:                WaitingState,
		userEventQueue:      map[User][]SiteEvent{},
		nextMessageID:       0,
		messageHandlers:     map[uint64]SiteEvent{},
		eventFinishHandlers: map[User]uint64{},
	}
}

// Name returns the name of the current state.
func (s *SiteVisitController) Name() GameState { return s.name }

func ShuffleQueue(e []SiteEvent) {
	rand.Shuffle(len(e), func(i, j int) { e[i], e[j] = e[j], e[i] })
}

// Begin is called when the state becomes active.
func (s *SiteVisitController) Begin() {
	// Fill up the queues with random events.
	for user, _ := range s.game.UserSites {
		for i := 0; i < MaxEventsPerRound; i++ {
			event := GenerateEvent(s.game, user)
			if event == nil {
				continue
			}

			s.userEventQueue[user] = append(
				s.userEventQueue[user],
				*event,
			)
		}
	}

	// The observed attack mechanism is handled here. All visitors at the
	// watchtower will get the observed attack message (and if no user is
	// there, the attacks will automatically proceed without defense).
	for i := 0; i < MaxEventsPerRound; i++ {
		event := GenerateObservedAttack(s.game, NewPlayer())
		if event == nil {
			continue
		}

		// We got an observed attack. If there are observers at the
		// watchtower, let one of them defend.
		possibleDefenders := []User{}
		for user, site := range s.game.UserSites {
			if site == Watchtower {
				possibleDefenders = append(possibleDefenders, user)
			}
		}

		// If there are no observers, add attacks to the users at that
		// site.
		if len(possibleDefenders) == 0 {
			for user, site := range s.game.UserSites {
				if site == event.site {
					s.userEventQueue[user] = append(s.userEventQueue[user], NewAttack())
				}
			}
		} else {
			// There are defenders. Choose one defender and let them defend it.
			defender := possibleDefenders[rand.Intn(len(possibleDefenders))]
			s.userEventQueue[defender] = append(s.userEventQueue[defender], event)
		}
	}

	// Shuffle all user event queues to make them seem more natural.
	for user, _ := range s.game.UserSites {
		ShuffleQueue(s.userEventQueue[user])
	}

	// Prepend the repair event to the user queue.
	for user, _ := range s.game.UserSites {
		s.userEventQueue[user] = append(
			[]SiteEvent{NewRepairSite()},
			s.userEventQueue[user]...,
		)
	}

	// Give all the users their initial events.
	s.HandlePhase()
}

// GiveNewEvent tries to give a user a new event from their queue. If there
// aren't any events, it returns false.
func (s *SiteVisitController) GiveNewEvent(u User) bool {
	if len(s.userEventQueue[u]) == 0 {
		fmt.Printf("No events for %q.", u.Name())
		return false
	}

	// Pop the event out of the user's queue
	event := s.userEventQueue[u][0]
	s.userEventQueue[u] = s.userEventQueue[u][1:]

	msg := event.Begin(s.game, u)

	// Register the message handler
	msg.MessageID = s.nextMessageID
	s.nextMessageID += 1
	s.messageHandlers[msg.MessageID] = event

	// If no subsequent follow-on message exists, the timeout
	// is actually the sum of the round duration + status
	timer := SiteVisitRoundDuration + SiteVisitStatusDuration

	if msg.HasSubsequentStatusUpdate {
		s.eventFinishHandlers[u] = msg.MessageID
		timer = SiteVisitRoundDuration
	}

	// Inform the user how long this event will take to handle.
	u.Message(NewSetClockMessage(timer))

	// Send the message to the user.
	u.Message(msg)

	return true
}

// End is called when the state is no longer active.
func (s *SiteVisitController) End() {}

func (s *SiteVisitController) HandleStatusPhase() {
	for u, i := range s.eventFinishHandlers {
		responder, ok := s.messageHandlers[i]
		// If the user already responded, the responder will have been
		// deleted, and we don't need to take any action here.
		if ok {
			response := responder.End(s.game, u, EventResponseMessage{})
			delete(s.messageHandlers, i)

			// Some events have follow-on short status updates. If so,
			// send the status update to the user immediately.
			if response != nil {
				u.Message(response)
				u.Message(NewSetClockMessage(SiteVisitStatusDuration))
			}
		}
	}
	s.game.SetTimeout(SiteVisitStatusDuration)
}

func (s *SiteVisitController) HandleEventPhase() {
	// Send everyone a new event
	noEventsLeft := true
	for user, _ := range s.game.UserSites {
		if s.GiveNewEvent(user) {
			noEventsLeft = false
		}
	}

	if noEventsLeft {
		s.game.ChangeState(SiteSelectionState)
		return
	}

	// Set another timer.
	s.game.SetTimeout(SiteVisitRoundDuration)
}

func (s *SiteVisitController) HandlePhase() {
	if s.statusPhase {
		s.HandleStatusPhase()
	} else {
		s.HandleEventPhase()
	}
	s.statusPhase = !s.statusPhase
}

// Timer is called when a timeout occurs.
func (s *SiteVisitController) Timer(tick time.Duration) {
	s.HandlePhase()
}

// RecieveMessage is called when a user sends a message to the server.
func (s *SiteVisitController) RecieveMessage(u User, m Message) {
	switch msg := m.(type) {
	case EventResponseMessage:
		// It's possible that the responder has already been called
		// due to a timer running over. So don't double-handle the event -
		// just ignore the response.
		responder, ok := s.messageHandlers[msg.MessageID]
		if ok {
			response := responder.End(s.game, u, msg)
			delete(s.messageHandlers, msg.MessageID)

			// Some events have follow-on short status updates. If so,
			// send the status update to the user immediately.
			if response != nil {
				u.Message(response)
			}
		}
		break
	case DefenseFailedMessage:
		// The watchtower failed to defend an attack. So it will propagate
		// to the recipients of the attack.
		for user, site := range s.game.UserSites {
			if site == msg.Site {
				// Prepend the attack so they definitely get it next round
				s.userEventQueue[user] = append([]SiteEvent{NewAttack()}, s.userEventQueue[user]...)
			}
		}
	default:
		return
	}
}

// NewStateController creates a state controller based on the requested state.
func NewStateController(game *Game, state GameState) StateController {
	switch state {
	case WaitingState:
		return NewWaitingController(game)
	case SiteSelectionState:
		return NewSiteSelectionController(game)
	case SiteVisitState:
		return NewSiteVisitController(game)
	default:
		panic("Unknown state!")
	}
}
