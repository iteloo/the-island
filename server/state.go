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
		// TODO: save this somehow
		fmt.Println("message: %d", msg)
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
	default:
		panic("Unknown state!")
	}
}
