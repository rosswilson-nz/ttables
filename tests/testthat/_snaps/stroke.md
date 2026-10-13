# stroke object prints correctly

    Code
      stroke("red")
    Output
      stroke: red

---

    Code
      stroke(thickness = "1pt")
    Output
      stroke: 1pt

---

    Code
      stroke(pattern = "dashed")
    Output
      stroke: dashed

---

    Code
      stroke("yellow", "1cm")
    Output
      stroke: yellow + 1cm

---

    Code
      stroke("yellow", pattern = "dotted")
    Output
      stroke: yellow + dotted

---

    Code
      stroke(thickness = "5mm", pattern = "solid")
    Output
      stroke: 5mm + solid

---

    Code
      stroke("blue", ".5in", "loosely-dashed")
    Output
      stroke: blue + 0.5in + loosely-dashed

