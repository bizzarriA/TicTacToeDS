import csv
import random

def check_winner(board):
    # Check rows, columns, and diagonals for a winner
    for i in range(3):
        if board[i][0] == board[i][1] == board[i][2] != 'b':
            return board[i][0]  # Winner in the row
        if board[0][i] == board[1][i] == board[2][i] != 'b':
            return board[0][i]  # Winner in the column

    if board[0][0] == board[1][1] == board[2][2] != 'b':
        return board[0][0]  # Winner in the main diagonal

    if board[0][2] == board[1][1] == board[2][0] != 'b':
        return board[0][2]  # Winner in the other diagonal

    return None  # No winner

def generate_random_board():
    symbols = ['X', 'O', 'X', 'O', 'X', 'O', 'X', 'O', 'X']
    board = [['b' for _ in range(3)] for _ in range(3)]

    for i in range(9):
        row, col = divmod(i, 3)
        symbol = random.choice(symbols)
        board[row][col] = symbol
        winner = check_winner(board)
        if winner:
            return board, winner
        symbols.remove(symbol)
    if valid_game(board):
        return board, 'b'
    return board, None

def valid_game(board):
    count_x = sum(row.count('X') for row in board)
    count_o = sum(row.count('O') for row in board)

    if count_x == count_o or count_x == count_o + 1:
        return True
    else:
        return False


def save_to_csv(filename, data):
    with open(filename, 'w', newline='') as csvfile:
        csv_writer = csv.writer(csvfile)
        csv_writer.writerows(data)

if __name__ == "__main__":
    num_games = 10000
    csv_filename = 'tictactoe_games.csv'

    game_data = []
    for _ in range(num_games):
        board, winner = generate_random_board()
        if winner:
            game_data.append([winner] + [cell for row in board for cell in row])

    header = ['Winner'] + [f'C{i}' for i in range(1, 10)]
    save_to_csv(csv_filename, [header] + game_data)
