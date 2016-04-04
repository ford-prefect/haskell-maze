# Maze Generator/Solver

A simple Maze generator and solver written in Haskell as a learning exercise.
Output looks like:

```
*---*---*---*---*---*---*---*---*---*---*---*---*---*---*---*---*---*---*---*---*
| →   →   →   →   ↓ | →   ↓                             | →   →   ↓         |   |
*   *---*---*---*   *   *   *---*   *---*---*---*---*---*   *---*   *---*   *   *
|   |           | →   ↑ | ↓ |       | →   →   →   →   →   ↑     | ↓ |   |   |   |
*   *---*   *---*---*---*   *---*---*   *---*---*---*---*---*---*   *   *   *   *
|       |   |           | →   →   →   ↑ |           | ↓   ← | ↓   ← |           |
*---*   *   *   *   *---*---*---*---*---*---*   *   *   *   *   *---*---*---*   *
|       |       |           |                   |   | ↓ | ↑   ← |       |       |
*   *---*---*   *---*   *   *---*---*---*   *---*   *   *---*   *   *   *   *---*
|           |   |       |       |       |   |   |   | →   ↓ |   |   |   |   |   |
*---*---*   *---*   *---*---*   *   *   *   *   *   *---*   *   *   *---*   *   *
|       |           |       |       |   |   |           | ↓ |   |       |       |
*   *   *---*---*---*   *---*---*---*   *   *   *---*---*   *---*---*   *---*   *
|   |               |                   |   |   | ↓   ←   ← |       |       |   |
*   *---*   *   *---*   *---*---*---*---*   *---*   *---*---*   *   *---*   *---*
|       |   |   |       |                   | ↓   ← |       |   |       |       |
*---*---*   *---*   *---*   *---*---*   *   *   *---*   *   *   *---*   *---*   *
|       |   |       |               |   |   | ↓ |       |   |   |   |           |
*   *   *   *   *---*---*---*---*---*   *   *   *   *---*   *   *   *---*---*---*
|   |       |   |           |       |   |   | ↓ |   |   |       |     →   →   ↓ |
*   *---*---*   *---*   *   *   *   *   *---*   *   *   *---*---*---*   *---*   *
|   |       |       |   |   |   |       | ↓   ← |                   | ↑ | ↓   ← |
*   *   *   *---*   *   *   *   *---*   *   *---*---*---*---*---*   *   *   *---*
|       |               |       |       | →   →   →   →   →   →   →   ↑ | →   👽 |
*---*---*---*---*---*---*---*---*---*---*---*---*---*---*---*---*---*---*---*---*
```