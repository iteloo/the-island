package main

import (
	"log"
	"time"
)

type Site string

const (
	NoSiteSelected Site = ""
	Forest         Site = "forest"
	Farm           Site = "farm"
	Hospital       Site = "hospital"
	Watchtower     Site = "watchtower"
	Beach          Site = "beach"
)

func AllSites() []Site {
	return []Site{Forest, Farm, Hospital, Watchtower, Beach}
}

const InitialRepairState uint64 = 50

type CommodityType string

const (
	Log     CommodityType = "log"
	Food    CommodityType = "food"
	Bandage CommodityType = "bandage"
	Bullet  CommodityType = "bullet"
)

var AllCommodities []CommodityType = []CommodityType{Log, Food, Bandage, Bullet}

const (
	// TradeTimeout specifies how long a trade can hang without a
	// counterpart before it is cancelled.
	TradeTimeout time.Duration = 100 * time.Millisecond
)

// User represents a single connection to a player, e.g. a websocket.
type User interface {
	Message(message Message) error
	Name() string
	SetName(name string)
}

// GameConnection holds a list of all the active players, and can be
// used to broadcast messages to all players.
type GameConnection interface {
	Broadcast(message Message) error
}

// Game represents the state of an individual game instance.
type Game struct {
	name            string
	connection      GameConnection
	state           StateController
	nextTimeout     time.Duration
	tick            time.Duration
	MinPlayers      int
	Yield           map[CommodityType]float64
	UserSites       map[User]Site
	SiteRepairState map[Site]uint64

	// The user that is proposing a trade right now.
	stagedUser      User
	stagingTime     time.Duration
	stagedMaterials string
}

// NewGame constructs a game.
func NewGame(name string, connection GameConnection) *Game {
	repair_state := map[Site]uint64{}
	for _, s := range AllSites() {
		repair_state[s] = InitialRepairState
	}

	game := Game{
		name:            name,
		connection:      connection,
		state:           nil,
		Yield:           make(map[CommodityType]float64),
		MinPlayers:      MinPlayers,
		UserSites:       map[User]Site{},
		SiteRepairState: repair_state,
	}
	game.state = NewStateController(&game, WaitingState)
	game.state.Begin()

	for _, c := range AllCommodities {
		game.Yield[c] = 1.00
	}

	return &game
}

// SetTimeout sets a time, after which the callback (state.Timer())
// on the currently active state will be invoked. Only one timer can
// be active at a time, and the callback will only occur in increments
// of the tick interval.
func (g *Game) SetTimeout(duration time.Duration) {
	g.nextTimeout = g.tick + duration
}

// GetTime returns the current time since the game began.
func (g *Game) GetTime() time.Duration {
	return g.tick
}

// Tick is called each time that the tick interval elapses.
func (g *Game) Tick(time time.Duration) {
	g.tick = time

	// If a timer is currently set, notify the state controller.
	if g.nextTimeout != 0 && time > g.nextTimeout {
		g.nextTimeout = 0
		g.state.Timer(time)
	}
}

// RecieveMessage is called when a user sends a message to the server.
func (g *Game) RecieveMessage(user User, message Message) {
	switch msg := message.(type) {
	case JoinMessage:
		user.Message(NewWelcomeMessage(g.name, string(g.state.Name())))
		g.UserSites[user] = NoSiteSelected
	case LeaveMessage:
		delete(g.UserSites, user)
	case SetNameMessage:
		user.SetName(msg.Name)

	case TradeMessage:
		isntSelfTrade := g.stagedUser != user
		withinTimeInterval := g.GetTime()-g.stagingTime < TradeTimeout
		if isntSelfTrade && g.stagedUser != nil && withinTimeInterval {
			// Execute the currently proposed trade.
			g.stagedUser.Message(NewTradeCompletedMessage(msg.Materials))
			user.Message(NewTradeCompletedMessage(g.stagedMaterials))

			// Reset the staged materials
			g.stagedUser = nil
			g.stagingTime = 0
			g.stagedMaterials = ""
		} else {
			g.stagedUser = user
			g.stagingTime = g.GetTime()
			g.stagedMaterials = msg.Materials
		}
	}
	g.state.RecieveMessage(user, message)
}

// ChangeState can be called by the state to transition to a new state.
func (g *Game) ChangeState(newState GameState) {
	g.state.End()

	log.Printf("State changed from %q to %q", g.state.Name(), newState)

	// Clean up any timers that are currently running
	g.nextTimeout = 0

	g.connection.Broadcast(NewGameStateChangedMessage(newState))
	g.state = NewStateController(g, newState)
	g.state.Begin()
}
