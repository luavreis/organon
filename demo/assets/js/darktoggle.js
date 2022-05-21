function darkToggle() {
    if (document.documentElement.classList.contains("dark")) {
        document.documentElement.classList.remove("dark")
        document.querySelectorAll('.dark-mode object').forEach(e => {
            e.data = "assets/icons/darkmode.svg"
        })
        document.getElementById("menu-icon").getSVGDocument()
                .querySelector("path").setAttribute("fill", "#b09f80")
    } else {
        document.documentElement.classList.add("dark")
        document.querySelectorAll('.dark-mode object').forEach(e => {
            e.data = "assets/icons/darkmode-on.svg"
        })
        document.getElementById("menu-icon").getSVGDocument()
                .querySelector("path").setAttribute("fill", "#f2e7d3")
    }
}
