var WebSocket = require('ws');

function connect(name, game, messages) {
  messages[name] = [];
  return new Promise((resolve, reject) => {
    var ws = new WebSocket("ws://localhost:8080/join?name=" + name +
                           "&game=" + game);
    ws.onopen = () => resolve(ws);
    ws.onmessage = (msg) => { messages[name].push(JSON.parse(msg.data)); };
    ws.onerror = (err) => reject(err);
  });
}

function sleep(ms) { return new Promise(r => setTimeout(r, ms)); }

function messages_to_string(messages) {
  var output = "";
  for (var i = 0; i < messages.length; i++) {
    output += "\t" + JSON.stringify(messages[i]) + "\n";
  }
  if (output == "") {
    output = "\t<no output>";
  }
  return output;
}

async function run_test(test_name, input, expected_output) {
  var messages = {};
  var names = [];
  for (var i = 0; i < input.length; i++) {
    if (!names.includes(input[i][0])) {
      names.push(input[i][0]);
    }
  }

  var connections = {};
  for (var i = 0; i < names.length; i++) {
    connections[names[i]] = await connect(names[i], test_name, messages);
  }

  for (var i = 0; i < input.length; i++) {
    connections[input[i][0]].send(input[i][1]);
    await sleep(50);
  }

  var errors = [];
  for (var i = 0; i < names.length; i++) {
    var want = messages_to_string(expected_output[names[i]]);
    var got = messages_to_string(messages[names[i]]);

    if (got != want) {
      errors.push("TEST FAILED [" + test_name + "] user=" + names[i] +
                  ":\ngot:\n" + got + "\nwant:\n" + want);
    }

    for (var i = 0; i < names.length; i++) {
      connections[names[i]].close();
    }

    if (errors.length > 0) {
      throw(errors.join("\n\n"));
    }

    console.log("TEST PASSED [" + test_name + "]");
  }
}

async function run_tests(tests) {
  for (var i = 0; i < tests.length; i++) {
    await run_test(tests[i].name, tests[i].input, tests[i].output);
  }
}

TEST_DATA = [ {
  "name" : "check_game_ready",
  "input" : [
    [ "colin", '{"action": "ready"}' ],
    [ "leo", '{"action": "ready"}' ],
  ],
  "output" : {
    "colin" : [
      {"action" : "welcome", "game" : "check_game_ready", "state" : "waiting"},
      {"action" : "game_state_changed", "new_state" : "production"}
    ],
    "leo" : [
      {"action" : "welcome", "game" : "check_game_ready", "state" : "waiting"},
      {"action" : "game_state_changed", "new_state" : "production"}
    ]
  }
} ];

run_tests(TEST_DATA)
    .catch((err) => console.log(err))
    .then(() => process.exit());
