package main

import (
	"log"
	"time"

	"github.com/gorilla/websocket"
)

const (
	// TickInterval is the nominal time between ticks. All timing is done in
	// increments of the TickInterval. It's kind of like the frame rate.
	TickInterval time.Duration = 300 * time.Millisecond
)

// Player is an implementation of User with websockets.
type Player struct {
	name       string
	Connection *websocket.Conn
}

func (p *Player) Name() string {
	return p.name
}

func (p *Player) SetName(name string) {
	p.name = name
}

// Message sends a player a message.
func (p *Player) Message(message Message) error {
	return p.Connection.WriteJSON(message)
}

// GenerateGameName generates a random name for the game, in case
// the user didn't specify one when they connected.
func GenerateGameName() string {
	return "test"
}

// An Event is a combination of a Message and the Player who originated the
// message.
type Event struct {
	Message Message
	Player  *Player
}

// NewEvent constructs an Event.
func NewEvent(player *Player, message Message) Event {
	return Event{
		Player:  player,
		Message: message,
	}
}

// A GameServer is an instance of a GameConnection.
type GameServer struct {
	players          []Player
	game             *Game
	incomingMessages chan Event
}

// Broadcast sends a message to every Player.
func (s *GameServer) Broadcast(message Message) error {
	log.Printf("Broadcast: %v", message)
	for _, p := range s.players {
		err := p.Message(message)
		if err != nil {
			log.Printf("Write failed during broadcast: %v\n", err)
		}
	}
	return nil
}

// AddPlayer is called by the main thread to add a player to our game. In fact, it
// queues a JoinMessage from this new player, which our game thread picks up.
func (s *GameServer) AddPlayer(player Player) {
	log.Printf("Adding new player %q to game %q", player.Name(), s.game.name)

	s.incomingMessages <- NewEvent(&player, NewJoinMessage())
}

// HandleCommunication is the main game loop which reads messages from players.
// This thread is where all of the game state logic is called from, including
// timer callbacks, etc.
func (s *GameServer) HandleCommunication(player Player) {
	// Send a join message as we arrive.
	s.incomingMessages <- NewEvent(&player, NewJoinMessage())

	for {
		t, data, err := player.Connection.ReadMessage()
		if err != nil {
			log.Printf("Websocket[name=%v] read error: %v", player.Name(), err)
			s.incomingMessages <- NewEvent(&player, NewLeaveMessage())
			return
		}

		if t != websocket.TextMessage {
			log.Printf("Websocket[name=%v] sent binary message", player.Name())
		}

		msg, err := DecodeMessage(data)
		log.Printf("Player[name=%v] sent message: %v", player.Name(), msg)
		if err != nil {
			log.Printf("Websocket[name=%v] sent invalid message: %v", player.Name(), err)
		}
		s.incomingMessages <- NewEvent(&player, msg)
	}
}

// HandleMessages is called on a new thread, once for each Player. It simply gets
// messages from the Player and sends them over to the game thread to be handled.
func (s *GameServer) HandleMessages() {
	for {
		event := <-s.incomingMessages
		switch msg := event.Message.(type) {
		case TickMessage:
			s.game.Tick(time.Duration(msg.Tick) * time.Millisecond)
		case JoinMessage:
			new := true
			for _, x := range s.players {
				if *event.Player == x {
					new = false
					break
				}
			}

			if new {
				// On the first pass, set up the player and begin handling
				// their messages for them.
				s.players = append(s.players, *event.Player)
				go s.HandleCommunication(*event.Player)
			} else {
				// On subsequent passes, we just want to send the message
				// through to the game controller.
				s.game.RecieveMessage(event.Player, event.Message)
			}
		default:
			s.game.RecieveMessage(event.Player, event.Message)
		}
	}
}

// RunClock is a dedicated thread which sends tick messages at the TickInterval.
func (s *GameServer) RunClock() {
	ticks := 0 * time.Second
	for {
		time.Sleep(TickInterval)
		ticks += TickInterval
		s.incomingMessages <- NewEvent(nil, NewTickMessage(ticks))
	}
}

// NewGameServer constructs a game server object, initializes the threads which it
// needs to handle messages and the game clock.
func NewGameServer(name string) *GameServer {
	g := GameServer{
		game:             nil,
		incomingMessages: make(chan Event),
	}
	g.game = NewGame(name, &g)

	go g.HandleMessages()
	go g.RunClock()

	return &g
}
