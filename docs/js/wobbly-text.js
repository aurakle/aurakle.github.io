let elements = document.querySelectorAll("span.wobbly");

elements.forEach((element) => {
    let text = element.innerText;
    element.innerText = "";

    for (const ch of text) {
        let wobblySpan = document.createElement("span");

        wobblySpan.style.display = "inline-block";
        wobblySpan.style.animation = `${Math.random() * 0.4}s spin linear infinite ${-Math.random() * 0.2}s alternate`;
        wobblySpan.style.transform = `translate(${(Math.random() * 2 - 1) * 0.1}em, ${(Math.random() * 2 - 1) * 0.1}em)`;
        wobblySpan.innerText = ch;

        element.appendChild(wobblySpan);
    }
})
