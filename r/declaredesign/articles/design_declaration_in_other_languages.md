# Design Diagnosis in other languages

| Language | Declaration in code | Figure based on mock data | Diagnosis |
|----|----|----|----|
| R | [![](https://declaredesign.org/other_languages/figures/two_arm_design_r_declaration.png)](https://declaredesign.org/other_languages/two_arm_design_r.r) | [![](https://declaredesign.org/other_languages/figures/two_arm_design_r.png)](https://declaredesign.org/other_languages/two_arm_design_r_figure.r) | [![](https://declaredesign.org/other_languages/figures/two_arm_design_r_diagnosis.png)](https://declaredesign.org/other_languages/two_arm_design_r.html) |
| Stata | [![](https://declaredesign.org/other_languages/figures/two_arm_design_stata_declaration.png)](https://declaredesign.org/other_languages/two_arm_design_stata.do) | [![](https://declaredesign.org/other_languages/figures/two_arm_design_stata.png)](https://declaredesign.org/other_languages/two_arm_design_stata.txt) | [![](https://declaredesign.org/other_languages/figures/two_arm_design_stata_diagnosis.png)](https://declaredesign.org/other_languages/two_arm_design_stata.html) |
| Python | [![](https://declaredesign.org/other_languages/figures/two_arm_design_python_declaration.png)](https://declaredesign.org/other_languages/two_arm_design_python.py) | [![](https://declaredesign.org/other_languages/figures/two_arm_design_python.png)](https://declaredesign.org/other_languages/two_arm_design_python_figure.py) | [![](https://declaredesign.org/other_languages/figures/two_arm_design_python_diagnosis.png)](https://declaredesign.org/other_languages/two_arm_design_python.html) |
| Excel | [![](https://declaredesign.org/other_languages/figures/two_arm_design_excel_declaration.png)](https://declaredesign.org/other_languages/two_arm_design_excel.xlsx) | [![](https://declaredesign.org/other_languages/figures/two_arm_design_excel.png)](https://declaredesign.org/other_languages/two_arm_design_excel.xlsx) | [![](https://declaredesign.org/other_languages/figures/two_arm_design_excel_diagnosis.png)](https://declaredesign.org/other_languages/two_arm_design_r.html) |

Design declaration in four languages {.table}

Click on the declarations to download the code files; the figures to
download the code that generated them; and the diagnosis to download a
reproducible document that includes the diagnosis. You can also download
the code for the reproducible documents for
[R](https://declaredesign.org/other_languages/figures/two_arm_design_r.rmd),
[Stata](https://declaredesign.org/other_languages/figures/two_arm_design_stata.txt),
and
[Python](https://declaredesign.org/other_languages/figures/two_arm_design_python.md).

## How to compile the reproducible documents

R: the .rmd is knit in Rstudio

Python: compiled through pandoc using the following command:

`stitch two_arm_design_python.md -o two_arm_design_python.html`

Stata: compiled from within Stata via the command:

`dyndoc two_arm_design_stata.txt, replace`
