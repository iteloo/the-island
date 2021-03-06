package main

import (
	"encoding/json"
	"fmt"
	"time"
)

// MessageAction is the string used in the `action` field
// within the JSON of the message. Each message has a unique
// action string.
type MessageAction string

const (
	// Server broadcast messages
	GameStateChangedAction MessageAction = "game_state_changed"
	WelcomeAction          MessageAction = "welcome"
	SetClockAction         MessageAction = "set_clock"
	PlayerInfoUpdateAction MessageAction = "player_info_updated"
	EventAction            MessageAction = "event"

	// Server-to-client messages
	TradeCompletedAction MessageAction = "trade_completed"

	// Client messages
	ReadyAction         MessageAction = "ready"
	JoinAction          MessageAction = "join_game"
	LeaveAction         MessageAction = "leave"
	DeathAction         MessageAction = "death"
	TradeAction         MessageAction = "trade"
	SetNameAction       MessageAction = "set_name"
	SiteSelectionAction MessageAction = "site_selected"
	GoBeachAction       MessageAction = "go_beach"
	EventResponseAction MessageAction = "event_response"

	// Special debug-only actions
	TickAction          MessageAction = "tick"
	DefenseFailedAction MessageAction = "defense_failed"
)

// A Message is an object which must contain an Action string, serializable
// to the MessageAction, and may also contain other JSON serializable fields.
type Message interface {
	requiresAlive() bool
}

// BasicMessage is a dummy message. All JSON messages sent or recieved by the
// server should be deserializable into this message type. This allows us to
// read the Action string without knowing the internal structure of the
// message.
type BasicMessage struct {
	Action string `json:"action"`
}

func (m BasicMessage) requiresAlive() bool { return false }

// TickMessage is sent to increment the current game clock. Users shouldn't send
// this message, it is only generated internally.
type TickMessage struct {
	Action string  `json:"action"`
	Tick   float64 `json:"tick_ms"`
}

func NewTickMessage(tick time.Duration) TickMessage {
	return TickMessage{
		Action: string(TickAction),
		Tick:   float64(tick / time.Millisecond),
	}
}

func (m TickMessage) requiresAlive() bool { return false }

// Messages broadcast by the server.

type GameStateChangedMessage struct {
	Action   string `json:"action"`
	NewState string `json:"new_state"`
}

func NewGameStateChangedMessage(newState GameState) Message {
	return GameStateChangedMessage{
		Action:   string(GameStateChangedAction),
		NewState: string(newState),
	}
}

func (m GameStateChangedMessage) requiresAlive() bool { return true }

type SetClockMessage struct {
	Action string `json:"action"`
	Time   int    `json:"time"`
}

func NewSetClockMessage(t time.Duration) Message {
	return SetClockMessage{
		Action: string(SetClockAction),
		Time:   int(t / time.Millisecond),
	}
}

func (m SetClockMessage) requiresAlive() bool { return false }

type PlayerInfo struct {
	Name  string `json:"name"`
	Ready bool   `json:"ready"`
}

type PlayerInfoUpdateMessage struct {
	Action string       `json:"action"`
	Info   []PlayerInfo `json:"info"`
}

func NewPlayerInfoUpdateMessage(info []PlayerInfo) Message {
	return PlayerInfoUpdateMessage{
		Action: string(PlayerInfoUpdateAction),
		Info:   info,
	}
}

func (m PlayerInfoUpdateMessage) requiresAlive() bool { return false }

type EventMessage struct {
	Action       string `json:"action"`
	MessageID    uint64 `json:"message_id"`
	Title        string `json:"title"`
	Description  string `json:"description"`
	HasOK        bool   `json:"has_ok"`
	OKButtonText string `json:"ok_button_text"`

	// If set, the event requires resolution early and will supply
	// a follow-up message
	HasSubsequentStatusUpdate bool `json:"has_subsequent_status_update"`

	// If set, modify the user's health by this much.
	HealthModifier int `json:"health_modifier"`

	// Must have enough resources to active action button. Positive
	// means user spends resource. Negative means user gets resource.
	HasActionButton            bool   `json:"has_action_button"`
	ActionButtonText           string `json:"action_button_text"`
	ActionButtonResource       string `json:"action_button_resource"`
	ActionButtonResourceAmount int    `json:"action_button_resource_amount"`

	// Spend button: user decides how much to spend
	HasSpendButton      bool          `json:"has_spend_button"`
	SpendButtonResource CommodityType `json:"spend_button_resource"`
}

func NewEventMessage(title, description string) EventMessage {
	return EventMessage{
		Action:       string(EventAction),
		Title:        title,
		Description:  description,
		OKButtonText: "OK",
		HasOK:        true,
	}
}

func (m EventMessage) requiresAlive() bool { return true }

func (m *EventMessage) WithOKButton(text string) *EventMessage {
	m.HasOK = true
	m.OKButtonText = text
	return m
}

func (m *EventMessage) WithActionButton(text string, resource CommodityType, amount int) *EventMessage {
	m.HasActionButton = true
	m.ActionButtonText = text
	m.ActionButtonResource = string(resource)
	m.ActionButtonResourceAmount = amount
	return m
}

func (m *EventMessage) WithResourceYield(text string, resource CommodityType, amount int) *EventMessage {
	return m.WithActionButton(text, resource, -amount)
}

func (m *EventMessage) WithSpendButton(resource CommodityType) {
	m.HasSpendButton = true
	m.SpendButtonResource = resource
}

// Server-to-client messages:

type TradeCompletedMessage struct {
	Action    string `json:"action"`
	Materials string `json:"materials"`
}

func NewTradeCompletedMessage(materials string) Message {
	return TradeCompletedMessage{string(TradeCompletedAction), materials}
}

func (m TradeCompletedMessage) requiresAlive() bool { return true }

type WelcomeMessage struct {
	Action string `json:"action"`
	Game   string `json:"game"`
	State  string `json:"state"`
}

func NewWelcomeMessage(game, state string) Message {
	return WelcomeMessage{
		Action: string(WelcomeAction),
		Game:   game,
		State:  state,
	}
}

func (m WelcomeMessage) requiresAlive() bool { return false }

// Client messages

type GoBeachMessage struct {
	Action    string                `json:"action"`
	Inventory map[CommodityType]int `json:"inventory"`
}

func NewGoBeachMessage(inventory map[CommodityType]int) Message {
	return GoBeachMessage{
		Action:    string(GoBeachAction),
		Inventory: inventory,
	}
}

func (m GoBeachMessage) requiresAlive() bool { return true }

type EventResponseMessage struct {
	MessageID      uint64 `json:"message_id"`
	ClickedOK      bool   `json:"clicked_ok"`
	ClickedAction  bool   `json:"clicked_action"`
	ResourceAmount int    `json:"resource_amount"`
}

func NewEventResponseMessage(id uint64, clicked_ok bool, clicked_action bool, amount int) EventResponseMessage {
	return EventResponseMessage{
		MessageID:      id,
		ClickedOK:      clicked_ok,
		ClickedAction:  clicked_action,
		ResourceAmount: amount,
	}
}

func (m EventResponseMessage) requiresAlive() bool { return false }

type ReadyMessage struct {
	Action string `json:"action"`
	Ready  bool   `json:"ready"`
}

func NewReadyMessage(ready bool) Message {
	return ReadyMessage{
		Action: string(ReadyAction),
		Ready:  ready,
	}
}

func (m ReadyMessage) requiresAlive() bool { return false }

type JoinMessage struct {
	Action string `json:"action"`
}

func NewJoinMessage() Message {
	return JoinMessage{string(JoinAction)}
}

func (m JoinMessage) requiresAlive() bool { return false }

type LeaveMessage struct {
	Action string `json:"action"`
}

func NewLeaveMessage() Message {
	return LeaveMessage{string(LeaveAction)}
}

func (m LeaveMessage) requiresAlive() bool { return false }

type DeathMessage struct {
	Action string `json:"action"`
}

func NewDeathMessage() Message {
	return DeathMessage{string(DeathAction)}
}

func (m DeathMessage) requiresAlive() bool { return true }

type TradeMessage struct {
	Action    string `json:"action"`
	Materials string `json:"materials"`
}

func NewTradeMessage(materials string) Message {
	return TradeMessage{
		Action:    string(TradeAction),
		Materials: materials,
	}
}

func (m TradeMessage) requiresAlive() bool { return true }

type SellMessage struct {
	Action   string `json:"action"`
	Quantity int64  `json:"quantity"`
	Type     string `json:"type"`
}

type SetNameMessage struct {
	Action string `json:"action"`
	Name   string `json:"name"`
}

func NewSetNameMessage(name string) SetNameMessage {
	return SetNameMessage{
		Action: string(SetNameAction),
		Name:   name,
	}
}

func (m SetNameMessage) requiresAlive() bool { return false }

type SiteSelectionMessage struct {
	Action       string `json:"action"`
	SiteSelected Site   `json:"site"`
}

func NewSiteSelectionMessage(site Site) SiteSelectionMessage {
	return SiteSelectionMessage{
		Action:       string(SiteSelectionAction),
		SiteSelected: site,
	}
}

func (m SiteSelectionMessage) requiresAlive() bool { return false }

// Internal-only messages
type DefenseFailedMessage struct {
	Action string `json:"action"`
	Site   Site   `json:"site"`
}

func NewDefenseFailedMessage(site Site) Message {
	return DefenseFailedMessage{
		Action: string(DefenseFailedAction),
		Site:   site,
	}
}

func (m DefenseFailedMessage) requiresAlive() bool { return true }

// DecodeMessage takes data in bytes, determines which message it corresponds
// to, and decodes it to the appropriate type.
func DecodeMessage(data []byte) (Message, error) {
	msg := BasicMessage{}
	if err := json.Unmarshal(data, &msg); err != nil {
		return nil, fmt.Errorf("Unable to decode message: %q", data)
	}

	// Now that we know the type of the message (based on the action) we
	// can decode it properly.
	var message Message
	var err error
	switch msg.Action {
	case string(GameStateChangedAction):
		m := GameStateChangedMessage{}
		err = json.Unmarshal(data, &m)
		message = m
	case string(WelcomeAction):
		m := WelcomeMessage{}
		err = json.Unmarshal(data, &m)
		message = m
	case string(TradeCompletedAction):
		m := TradeCompletedMessage{}
		err = json.Unmarshal(data, &m)
		message = m
	case string(ReadyAction):
		m := ReadyMessage{}
		err = json.Unmarshal(data, &m)
		message = m
	case string(JoinAction):
		m := JoinMessage{}
		err = json.Unmarshal(data, &m)
		message = m
	case string(LeaveAction):
		m := LeaveMessage{}
		err = json.Unmarshal(data, &m)
		message = m
	case string(DeathAction):
		m := DeathMessage{}
		err = json.Unmarshal(data, &m)
		message = m
	case string(TradeAction):
		m := TradeMessage{}
		err = json.Unmarshal(data, &m)
		message = m
	case string(TickAction):
		m := TickMessage{}
		err = json.Unmarshal(data, &m)
		message = m
	case string(SetNameAction):
		m := SetNameMessage{}
		err = json.Unmarshal(data, &m)
		message = m
	case string(GoBeachAction):
		m := GoBeachMessage{}
		err = json.Unmarshal(data, &m)
		message = m
	case string(EventResponseAction):
		m := EventResponseMessage{}
		err = json.Unmarshal(data, &m)
		message = m
	case string(SiteSelectionAction):
		m := SiteSelectionMessage{}
		err = json.Unmarshal(data, &m)
		message = m
	default:
		err = fmt.Errorf("Unknown action: %v", msg.Action)
	}

	return message, err
}
