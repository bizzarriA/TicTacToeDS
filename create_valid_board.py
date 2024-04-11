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

def generate_random_board(previous_board=None):
    symbols = ['X', 'O', 'X', 'O', 'X', 'O', 'X', 'O', 'X']
    board = [['b' for _ in range(3)] for _ in range(3)]
    winner = None
    for i in range(9):
        row, col = divmod(i, 3)
        symbol = random.choice(symbols)
        board[row][col] = symbol
        winner = check_winner(board)
        if winner:
            break
        symbols.remove(symbol)
    if valid_game(board):
        winner = 'b'
    if previous_board is not None:
        sample = [winner] + [cell for row in board for cell in row]
        if sample in previous_board:
            winner = None
    return board, winner

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
    num_games = 100000

    game_data = []
    i = 0
    while i in range(num_games):
        board, winner = generate_random_board()
        if winner:
            i += 1
            game_data.append([winner] + [cell for row in board for cell in row])

    # Split into train and test
    random.shuffle(game_data)
    train_size = int(0.8 * len(game_data))
    train_data = game_data[:train_size]
    test_data = game_data[train_size:]

    # Save train and test sets to separate CSV files
    header = ['Winner'] + [f'C{i}' for i in range(1, 10)]
    save_to_csv('data/labels/train.csv', [header] + train_data)
    save_to_csv('data/labels/test.csv', [header] + test_data)
