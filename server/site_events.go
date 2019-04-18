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

type Attack struct{}

func NewAttack() Attack {
	return Attack{}
}

func (e Attack) Mods(g *Game, u User) int {
	site := g.UserSites[u]

	switch site {
	case Forest:
		return 100
	case Farm:
		return 100
	case Hospital:
		return 100
	case Watchtower:
		return 50
	}

	return 0
}

func (e Attack) Begin(g *Game, u User) EventMessage {
	site := g.UserSites[u]
	var title string
	switch site {
	case Forest:
		title = fmt.Sprintf("A bear is attacking you!")
	case Farm:
		title = fmt.Sprintf("A rabbit is attacking you!")
	case Hospital:
		title = fmt.Sprintf("A bat is attacking you!")
	case Watchtower:
		title = fmt.Sprintf("An owl is attacking you!")
	}

	description := fmt.Sprintf("You have a chance to shoot it, if you have any bullets!")
	msg := NewEventMessage(title, description)
	msg.HasSubsequentStatusUpdate = true
	msg.WithSpendButton(Bullet)

	return msg
}

func (e Attack) End(g *Game, u User, r EventResponseMessage) *EventMessage {
	msg := NewEventMessage("The animal bites you!", "It's very painful!")
	msg.HealthModifier = -1

	if r.ResourceAmount > 0 {
		msg.Title = fmt.Sprintf("You shot the animal.")
		msg.Description = fmt.Sprintf("In an act of heroic bravery, you shot the animal and saved yourself")
		msg.HealthModifier = 0
	}

	return &msg
}

type GotoBeach struct{}

func NewGotoBeach() GotoBeach {
	return GotoBeach{}
}

func (e GotoBeach) Mods(g *Game, u User) int { return 0 }
func (e GotoBeach) Begin(g *Game, u User) EventMessage {
	title := fmt.Sprintf("You are at the beach")
	description := fmt.Sprintf("Looking around to see if anyone else is here...")
	msg := NewEventMessage(title, description)
	msg.HasSubsequentStatusUpdate = true
	return msg
}
func (e GotoBeach) End(g *Game, u User, r EventResponseMessage) *EventMessage {
	switch s := g.state.(type) {
	case *SiteVisitController:
		// [hack] recomputing for every users
		totalNumBeachGoers := len(s.goBeachResponses)
		totalResources := map[CommodityType]int{
			Log: 0, Food: 0, Bandage: 0, Bullet: 0,
		}
		for _, inventory := range s.goBeachResponses {
			for commodityType, count := range inventory {
				totalResources[commodityType] += count
			}
		}
		required := ResourceRequiredToLeave(totalNumBeachGoers)
		leaveSuccess := true
		for commodityType, count := range totalResources {
			if count < required[commodityType] {
				leaveSuccess = false
			}
		}
		title := fmt.Sprintf("Haha, you are stuck for now. Not enough resources")
		description := "Now you gotta go back and face the group"
		if leaveSuccess {
			title = fmt.Sprintf("Together, you have enough resources to leave the island!")
			description = "LEEEAVE NOW!"
		}
		msg := NewEventMessage(title, description)
		return &msg

	default:
		panic(fmt.Sprintf("GotoBeach event ending during %v", s))
	}
}

type ObserveAttack struct {
	site Site
}

func NewObserveAttack() ObserveAttack {
	sites := []Site{Forest, Farm, Hospital}
	choice := rand.Intn(len(sites))

	return ObserveAttack{
		site: sites[choice],
	}
}

func (e ObserveAttack) Mods(g *Game, u User) int {
	return 500
}

func (e ObserveAttack) Begin(g *Game, u User) EventMessage {
	title := "Animals on the move!"
	description := fmt.Sprintf("Angry animals are moving toward the %s. You can shoot them, if you have bullets.", e.site)
	msg := NewEventMessage(title, description)
	msg.WithSpendButton(Bullet)
	msg.HasSubsequentStatusUpdate = true

	return msg
}

func (e ObserveAttack) End(g *Game, u User, r EventResponseMessage) *EventMessage {
	if r.ResourceAmount > 0 {
		msg := NewEventMessage("You shot the animals!", "They ran away scared.")
		return &msg
	}

	// Send a defense failed message to the game.
	g.RecieveMessage(u, NewDefenseFailedMessage(e.site))

	title := fmt.Sprintf("The animals go straight for the %s!", e.site)
	description := "They look really angry!"
	msg := NewEventMessage(title, description)

	return &msg
}

func GenerateObservedAttack(g *Game, u User) *ObserveAttack {
	event := NewObserveAttack()
	choice := rand.Intn(1000)
	if choice < event.Mods(g, u) {
		return &event
	}

	return nil
}

func GenerateEvent(g *Game, u User) *SiteEvent {
	allEvents := []SiteEvent{
		NewGetResource(),
		NewAttack(),
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
