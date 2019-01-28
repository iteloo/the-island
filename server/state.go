package main

import (
	"fmt"
	"log"
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

	// Amount of time to wait while site is selected.
	SiteSelectionDuration time.Duration = 5 * time.Second
	// Amount of time to spend at the site when visiting.
	SiteVisitDuration time.Duration = 10 * time.Second
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

	userEventQueue  map[User][]SiteEvent
	nextMessageID   uint64
	messageHandlers map[uint64]SiteEvent
}

func NewSiteVisitController(game *Game) *SiteVisitController {
	return &SiteVisitController{
		game:            game,
		name:            WaitingState,
		userEventQueue:  map[User][]SiteEvent{},
		nextMessageID:   0,
		messageHandlers: map[uint64]SiteEvent{},
	}
}

// Name returns the name of the current state.
func (s *SiteVisitController) Name() GameState { return s.name }

// Begin is called when the state becomes active.
func (s *SiteVisitController) Begin() {
	s.game.connection.Broadcast(NewSetClockMessage(SiteVisitDuration))
	s.game.SetTimeout(SiteVisitDuration)

	// Set the initial user event queues up.
	for user, _ := range s.game.UserSites {
		s.userEventQueue[user] = append(
			s.userEventQueue[user],
			NewRepairSite(),
		)
	}

	// Try to give all the users their initial events.
	for user, _ := range s.game.UserSites {
		s.GiveNewEvent(user)
	}
}

func (s *SiteVisitController) GiveNewEvent(u User) {
	if len(s.userEventQueue[u]) == 0 {
		fmt.Printf("No events for %q.", u.Name())
		return
	}

	// Pop the event out of the user's queue
	event := s.userEventQueue[u][0]
	s.userEventQueue[u] = s.userEventQueue[u][1:]

	msg := event.Begin(s.game, u)

	// Register the message handler
	msg.MessageID = s.nextMessageID
	s.nextMessageID += 1
	s.messageHandlers[msg.MessageID] = event

	// Send the message to the user.
	u.Message(msg)
}

// End is called when the state is no longer active.
func (s *SiteVisitController) End() {}

// Timer is called when a timeout occurs.
func (s *SiteVisitController) Timer(tick time.Duration) {
	s.game.ChangeState(SiteSelectionState)
}

// RecieveMessage is called when a user sends a message to the server.
func (s *SiteVisitController) RecieveMessage(u User, m Message) {
	switch msg := m.(type) {
	case EventResponseMessage:
		// Let the SiteEvent handle the user response
		s.messageHandlers[msg.MessageID].End(s.game, u, msg)

		// Send the user the next message in their queue.
		s.GiveNewEvent(u)
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
