document.addEventListener("DOMContentLoaded", function() {
    let printButton = document.createElement("button");
    printButton.innerHTML = "📄 Print / Save as PDF";
    printButton.style.position = "fixed";
    printButton.style.bottom = "20px";
    printButton.style.right = "20px";
    printButton.style.padding = "10px 15px";
    printButton.style.fontSize = "16px";
    printButton.style.backgroundColor = "#007bff";
    printButton.style.color = "#fff";
    printButton.style.border = "none";
    printButton.style.borderRadius = "5px";
    printButton.style.cursor = "pointer";
    
    printButton.onclick = function() {
        window.print();
    };
    
    document.body.appendChild(printButton);
});
