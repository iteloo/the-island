package main

import (
	"fmt"
	"math/rand"
)

type SiteEvent interface {
	// The change of the event ocurring.
	Mods(*Game, User) int

	Begin(*Game, User) EventMessage

	// If the end message returns nil, no status update. If
	// it returns an EventMessage, it should be the status resulting
	// from the event decision.
	End(*Game, User, EventResponseMessage) *EventMessage
}

type RepairSite struct{}

func NewRepairSite() RepairSite {
	return RepairSite{}
}

func (e RepairSite) Mods(g *Game, u User) int { return 0 }
func (e RepairSite) Begin(g *Game, u User) EventMessage {
	site := g.UserSites[u]
	title := fmt.Sprintf("Repair %s?", site)
	description := fmt.Sprintf("The %s looks about %d%% functional.", site, g.SiteRepairState[site])

	msg := NewEventMessage(title, description)
	msg.WithSpendButton(Log)
	msg.HasSubsequentStatusUpdate = true
	return msg
}
func (e RepairSite) End(g *Game, u User, r EventResponseMessage) *EventMessage {
	site := g.UserSites[u]
	g.SiteRepairState[site] += uint64(r.ResourceAmount)

	if r.ResourceAmount > 0 {
		title := fmt.Sprintf("Repaired %s.", site)
		description := fmt.Sprintf("Thanks to your hard work, the %s now looks about %d%% functional.", site, g.SiteRepairState[site])
		msg := NewEventMessage(title, description)
		return &msg
	}

	title := fmt.Sprintf("Didn't repair %s.", site)
	description := fmt.Sprintf("You're so lazy")
	msg := NewEventMessage(title, description)
	return &msg
}

type GetResource struct{}

func NewGetResource() GetResource {
	return GetResource{}
}

func (e GetResource) Mods(g *Game, u User) int {
	// Only some sites can support picking up items.
	site := g.UserSites[u]

	switch site {
	case Forest:
		return 500
	case Farm:
		return 500
	case Hospital:
		return 500
	case Watchtower:
		return 500
	}

	return 0
}

func (e GetResource) Begin(g *Game, u User) EventMessage {
	site := g.UserSites[u]

	resource := Log
	term := "logs"

	switch site {
	case Forest:
		resource = Log
		term = "logs"
	case Farm:
		resource = Food
		term = "food"
	case Hospital:
		resource = Bandage
		term = "bandages"
	case Watchtower:
		resource = Bullet
		term = "bullets"
	}

	title := fmt.Sprintf("Found some %s", term)
	description := "You are lucky"

	msg := NewEventMessage(title, description)
	msg.WithResourceYield("Pick up", resource, 1)
	return msg
}

func (e GetResource) End(g *Game, u User, r EventResponseMessage) *EventMessage {
	return nil
}

func GenerateEvent(g *Game, u User) *SiteEvent {
	allEvents := []SiteEvent{
		NewGetResource(),
	}

	choice := rand.Intn(1000)
	count := 0
	for _, event := range allEvents {
		count += event.Mods(g, u)
		if choice < count {
			return &event
		}
	}

	return nil
}
