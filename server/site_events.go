package main

import "fmt"

type SiteEvent interface {
	// The change of the event ocurring.
	Mods(*Game, User) uint64

	Begin(*Game, User) EventMessage
	End(*Game, User, EventResponseMessage)
}

type RepairSite struct{}

func NewRepairSite() RepairSite {
	return RepairSite{}
}

func (e RepairSite) Mods(g *Game, u User) uint64 { return 0 }
func (e RepairSite) Begin(g *Game, u User) EventMessage {
	site := g.UserSites[u]
	title := fmt.Sprintf("Repair %s?", site)
	description := fmt.Sprintf("The %s looks about %d%% functional.", site, g.SiteRepairState[site])

	msg := NewEventMessage(title, description)
	msg.WithSpendButton(Log)
	return msg
}
func (e RepairSite) End(g *Game, u User, r EventResponseMessage) {
	site := g.UserSites[u]
	g.SiteRepairState[site] += uint64(r.ResourceAmount)
}
