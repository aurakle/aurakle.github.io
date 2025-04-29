let elements = document.querySelectorAll("span.wobbly");

elements.forEach((element) => {
    let text = element.innerText;
    element.innerText = "";

    for (const ch of text) {
        element.appendChild(nestedSpans(ch == " " ? "\xa0" : `${ch}`, 6));
    }
})

function nestedSpans(inner, depth) {
    for (let i = 0; i < Math.floor(Math.abs(depth)) + 1; i++) {
        let span = document.createElement("span");

        if (typeof inner === "string") {
            span.innerText = inner;
        } else {
            span.appendChild(inner);
        }

        span.style.display = "inline-block";
        span.style.animation = `${Math.random() * 0.4}s spin linear infinite ${-Math.random() * 0.2}s alternate`;
        span.style.transform = `translate(${(Math.random() * 2 - 1) * 0.08}em, ${(Math.random() * 2 - 1) * 0.1}em)`;

        inner = span;
    }

    return inner;
}
