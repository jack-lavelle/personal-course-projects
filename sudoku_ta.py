from typing import Tuple, List

import numpy as np
import numpy.typing as npt
import matplotlib.pyplot as plt
from random import sample

"""
Sudoku board initializer
Credit: https://stackoverflow.com/questions/45471152/how-to-create-a-sudoku-puzzle-in-python
"""
def generate(n: int, num_clues: int) -> dict:
    # Generate a sudoku problem of order n with "num_clues" cells assigned
    # Return dictionary containing clue cell indices and corresponding values
    # (You do not need to worry about components inside returned dictionary)
    N = range(n)

    rows = [g * n + r for g in sample(N, n) for r in sample(N, n)]
    cols = [g * n + c for g in sample(N, n) for c in sample(N, n)]
    nums = sample(range(1, n**2 + 1), n**2)

    S = np.array(
        [[nums[(n * (r % n) + r // n + c) % (n**2)] for c in cols] for r in rows]
    )
    indices = sample(range(n**4), num_clues)
    values = S.flatten()[indices]

    mask = np.full((n**2, n**4), True)
    mask[:, indices] = False
    i, j = np.unravel_index(indices, (n**2, n**2))

    for c in range(num_clues):
        v = values[c] - 1
        maskv = np.full((n**2, n**2), True)
        maskv[i[c]] = False
        maskv[:, j[c]] = False
        maskv[
            (i[c] // n) * n : (i[c] // n) * n + n, (j[c] // n) * n : (j[c] // n) * n + n
        ] = False
        mask[v] = mask[v] * maskv.flatten()

    return {"n": n, "indices": indices, "values": values, "valid_indices": mask}


def display(problem: dict):
    # Display the initial board with clues filled in (all other cells are 0)
    n = problem["n"]
    empty_board = np.zeros(n**4, dtype=int)
    empty_board[problem["indices"]] = problem["values"]
    print("Sudoku puzzle:\n", np.reshape(empty_board, (n**2, n**2)), "\n")


def initialize(problem: dict) -> npt.NDArray:
    # Returns a random initial sudoku board given problem
    n = problem["n"]
    S = np.zeros(n**4, dtype=int)
    S[problem["indices"]] = problem["values"]

    all_values = list(np.repeat(range(1, n**2 + 1), n**2))
    for v in problem["values"]:
        all_values.remove(v)
    all_values = np.array(all_values)
    np.random.shuffle(all_values)

    indices = [i for i in range(S.size) if i not in problem["indices"]]
    S[indices] = all_values
    S = S.reshape((n**2, n**2))

    return S


def successors(S: npt.NDArray, problem: dict) -> List[npt.NDArray]:
    # Returns list of all successor states of S by swapping two non-clue entries
    mask = problem["valid_indices"]
    indices = [i for i in range(S.size) if i not in problem["indices"]]
    succ = []

    for i in range(len(indices)):
        for j in range(i + 1, len(indices)):
            s = np.copy(S).flatten()
            if s[indices[i]] == s[indices[j]]:
                continue
            if not (
                mask[s[indices[i]] - 1, indices[j]]
                and mask[s[indices[j]] - 1, indices[i]]
            ):
                continue
            s[indices[i]], s[indices[j]] = s[indices[j]], s[indices[i]]
            succ.append(s.reshape(S.shape))

    return succ


"""
WRITE THIS FUNCTION
"""
def num_errors(S: npt.NDArray) -> int:
    # Given a current sudoku board state (2d NumPy array), compute and return total number of errors
    # Count total number of missing numbers from each row, column, and non-overlapping square blocks
    numbers = range(1, len(S[0]) + 1)
    row_errors = 0
    column_errors = 0
    nsquared_errors = 0

    #row iteration
    for row in S:
      numbers_seen = []
      for number in row:
        numbers_seen.append(number)

      
      
      
      row_missing_numbers = list(set(list(numbers)) - set(numbers_seen))
      row_errors += len(row_missing_numbers)

    #column iteration
    for i in range(0, len(S[0])):
      numbers_seen = []
      for number in range(0, len(S)):
        numbers_seen.append(S[number][i])

      column_missing_numbers = list(set(numbers) - set(numbers_seen))
      column_errors += len(column_missing_numbers)

    
    #print(column_errors)

    #ngrid iteration
    n = int(np.sqrt(len(numbers)))
    newS = np.reshape(S, (n**4))
    subgrid_errors = 0
    bigy = 0
    while bigy < n:
      bigx = 0
      while bigx < n:
          smally = 0
          subgrid_numbers_seen = []
          while smally < n:
              smallx = 0
              while smallx < n:
                  subgrid_numbers_seen.append(newS[(bigy * n**3)+(bigx * n) + smallx + (n**2 * smally)])
                  smallx += 1
              smally += 1
          subgrid_missing_numbers = list(set(numbers) - set(subgrid_numbers_seen))
          subgrid_errors += len(subgrid_missing_numbers)

          bigx += 1
      bigy += 1
    #print(subgrid_errors)

    return (subgrid_errors + column_errors + row_errors)


"""
WRITE THIS FUNCTION
"""
def hill_climb(
    problem: dict,
    max_sideways: int = 0,
    max_restarts: int = 0
) -> Tuple[npt.NDArray, List[int]]:
    # Given: Sudoku problem and optional max sideways moves and max restarts parameters
    # Return: Board state solution (2d NumPy array), list of errors in each iteration of hill climbing search
    myNPTarray = initialize(problem)
    errorlist = []
    sideway_counter = 0
    minerror = float("inf")
    restart_counter = 0

    while(num_errors(myNPTarray) > 0):
      olderror = num_errors(myNPTarray)
      mylistofsuccessors = successors(myNPTarray, problem)

      for i in mylistofsuccessors:
        if ((num_errors(i)) < minerror):
          myNPTarray = i
          minerror = num_errors(i)
          errorlist.append(minerror)

      if (olderror == minerror):

         if sideway_counter >= max_sideways:
           
           if restart_counter >= max_restarts:
            return myNPTarray, errorlist
            
           if restart_counter < max_sideways:
            myNPTarray = initialize(problem)
            sideway_counter = 0
            minerror = float("inf")
            restart_counter += 1
           

         if sideway_counter < max_sideways:
           for x in mylistofsuccessors:
             if (num_errors(x) == minerror):
              myNPTarray = x
              break
           sideway_counter += 1
           errorlist.append(minerror)

    if (num_errors(myNPTarray) == 0):
      return myNPTarray, errorlist
    
    

def runhill(iterations: int):
    n = 2
    clues = 5
    counter = 0
    errorlist = []
    while (counter < iterations):
        problem = generate(n, clues)
        sol, errors = hill_climb(problem, 10, 10)
        errorlist.append(errors[-1])
        counter += 1
    successes = 0
    for i in errorlist:
      if i == 0:
        successes += 1
    return (successes / iterations), (sum(errorlist) / 100)

if __name__ == "__main__":
    n = 3
    clues = 40
    problem = generate(n, clues)
    display(problem)
    sol, errors = hill_climb(problem, 10, 10)
    print("Solution:\n", sol)
    plt.plot(errors)
    plt.show()

    #success_rate, average_error = runhill(100)
    #print(success_rate)
    #print(average_error)


