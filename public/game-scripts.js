const o = document.getElementById("out");
const ws = new WebSocket(`ws://${location.host}/ws`);
const guess = document.getElementById("guess");
const submit = document.getElementById("submit");

const bracket1 = document.getElementById("bracket1");

ws.onmessage = (event) => {
    const msg = event.data;
    console.log(msg);

    if (msg.startsWith("BRACKET|")) {
        bracket1.textContent = "[" + msg.slice("BRACKET|".length) + "]";
    }
};

submit.onclick = () => {
    ws.send(guess.value);
    guess.value = "";
};

guess.addEventListener("keypress", (e) => {
    if (e.key === "Enter") {
        ws.send(guess.value);
        guess.value = "";
    }
});
