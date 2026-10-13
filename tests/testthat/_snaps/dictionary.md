# format.ttables_dictionary() prints correctly

    Code
      format(dictionary(a = 1, b = "2", c = 3:4))
    Output
      [1] "(a: 1, b: \"2\", c: 3)"

---

    Code
      dictionary(a = 1, b = "2", c = 3:4)
    Output
      (a: 1, b: "2", c: 3) 

