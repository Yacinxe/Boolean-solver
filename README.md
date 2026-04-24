# 🧠 Boolean Solver — OCaml

> Solveur logique complet : équations booléennes → génération d’environnements → évaluation → solutions

## 📋 Overview

This project implements a **Boolean equation solver in OCaml** capable of finding **all satisfying assignments** for a system of logical equations.

The solver works with:
- Boolean variables (X1, X2, …)
- Logical constants (True, False)
- Logical operators (AND, OR, NOT)

It generates all possible environments (2^n combinations) and filters those that satisfy the entire system.

---

## 🏗️ Architecture
