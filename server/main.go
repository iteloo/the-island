package main

import (
	"flag"
	"fmt"
	"github.com/gorilla/websocket"
	"log"
	"net/http"
)

var (
	// AllGames is a map of all the games currently in progress.
	// The key is the name of the game.
	AllGames map[string]*GameServer
)

var upgrader = websocket.Upgrader{
	ReadBufferSize:  1024,
	WriteBufferSize: 1024,
	CheckOrigin:     func(r *http.Request) bool { return true },
}

// The /join URL takes two parameters, game, and name. The game
// argument is optional. If specified, we'll try to join a game
// with that name.
func join(w http.ResponseWriter, r *http.Request) {
	params := r.URL.Query()
	n, ok := params["name"]
	name := "Anonymous"
	if ok {
		name = n[0]
	}

	t, ok := params["game"]
	var target string
	if ok {
		target = t[0]
	} else {
		target = GenerateGameName()
	}

	conn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		log.Println(err)
		return
	}

	player := Player{
		name:       name,
		Connection: conn,
	}

	game, ok := AllGames[target]
	if !ok {
		// The game doesn't exist, create it.
		game = NewGameServer(target)
		AllGames[target] = game
	}
	game.AddPlayer(player)
}

func main() {
	port := flag.String("port", "8080", "the port to use to serve")
	flag.Parse()

	AllGames = make(map[string]*GameServer)
	http.HandleFunc("/join", join)
	http.HandleFunc("/", http.FileServer(http.Dir("./web")).ServeHTTP)
	log.Fatal(http.ListenAndServe(fmt.Sprintf(":%s", *port), nil))

}
