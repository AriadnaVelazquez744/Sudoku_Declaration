# 🎯 Sudoku's Declaration🧩

Welcome to **Sudoku's Declaration**, an interactive Sudoku game developed using **Haskell** and **Gloss** for graphics. Additionally, it includes a **CSP-based solver (Constraint Satisfaction Problem)** that can solve partially filled boards if they are viable.

## 📌 **Features**

✅ **Intuitive Graphical Interface:** Built with `Gloss`, optimized design, and highlighted cells.\
✅ **Initial Board Generation:** Automatically generates a valid Sudoku at startup.\
✅ **User Interaction:**

- Select cells and edit numbers.
- Highlights selected cells (light purple) and initial cells (light gray).
- Error detection with incorrect numbers in red.

✅ **Smart Solver (CSP):** Can solve a partially filled board if it is valid.\
✅ **Custom Screens:**
- Main menu with options: **Play**, **Rules** and **Credits**.
- Game screen with buttons for **New Game**, **Clear Progress**, **Solve**, and **Continue to the next screen** in case of using the Solver.
- **End Game** screen upon successful completion of Sudoku.

---

## 🚀 **Installation**

### 🔧 **Requirements**

- Haskell (`ghc`, `cabal`, or `stack`)
- `Gloss` for graphics
- `GLUT` and `OpenGL` (for graphical support)

### 📥 **Clone the Repository**

```bash
git clone https://github.com/AriadnaVelazquez744/Sudoku_Declaration.git
cd Sudoku_Declaration
```

### 🛠 **Install Dependencies**

If using **cabal**:

```bash
cabal install gloss OpenGL GLUT
```

If using **stack**:

```bash
stack setup
stack install gloss OpenGL GLUT
```

---

## ▶️ **Running the Game**

To compile and run the game:

```bash
ghc -o sudoku Main.hs
./sudoku
```

If using **stack**:

```bash
stack run
```

---

## 🎮 **How to Play**

1️⃣ In the main menu, select **Play**.\
2️⃣ Interact with the board by clicking on an empty cell and entering a number (1-9).\
3️⃣ **Initial cells**: Light gray (not editable).\
4️⃣ **Selected cell**: Light purple.\
5️⃣ **Incorrect number**: Displayed a warning message in red with the number you try to put, while keeping the cell highlighted.\
6️⃣ **Solve Sudoku**: Click the **AutoSolver** button to automatically solve a viable board.\
7️⃣ **Finish the game**: If you complete the Sudoku correctly, the **Winner** screen appears.&#x20;

---

## 🏗 **Project Structure**

```
📁 sudoku-haskell/
│── 📄 Main.hs                   # Main file, manages screens and game logic
│── 📂 Graphics/
│   ├── 📄 Renderer.hs           # General screen rendering
│   ├── 📄 Board.hs              # Drawing the board and cells
│   ├── 📄 UI.hs                 # Buttons and styled text
│── 📂 Logic/
│   ├── 📄 GameLogic.hs          # Sudoku validation and mechanics
│   ├── 📄 CSP.hs                # CSP solver implementation
│── 📂 Events/
│   ├── 📄 Events.hs             # Handles all input events
│   ├── 📄 EventsHelpers.hs      # Methods to help handle the main events
│── 📂 Common/
│   ├── 📄 Types.hs              # Definition of types and data structures
|   📂 Img/
|   ├── title.bmp
|   ├── win.bmp
|   ├── matcom.bmp
|   ├── elements.bmp
│── 📄 README.md                 # This file
│── 📄 .gitignore                # Exclude all the files of Haskell compiling process
```

---

## 🧠 **Sudoku Solver (CSP)**

The **CSP solver** uses a constraint-based approach:

- **Variables**: Empty cells (`Nothing`).
- **Domains**: Numbers from 1 to 9.
- **Constraints**: No number repeats in rows, columns, or 3x3 subgrids.

### ✨ **Example Usage of the Solver**

If you have an incomplete board and want to solve it:

```haskell
import Logic.CSP (solveBoardWithCSP)

main :: IO ()
main = do
    let solvedBoard = solveBoardWithCSP currentBoard
    print solvedBoard

```

---

## 🖥 **Customization**

🔹 **Change colors**: Modify `Graphics.Board.hs` in `cellStyle`.\
🔹 **Change font and number style**: Modify `drawValue` in `Graphics.UI.hs`.\
🔹 **Add more screens**: Use `Graphics.Renderer.hs` to manage screen transitions.\
🔹 Ensure required images are placed in the `Img/` folder (e.g., `"title.bmp"`).\

---

## ✨ **Author**

Developed by **[Ariadna Velázquez and Lía Lopez]** as part of a **Declarative Programming** project.
