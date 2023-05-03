from typing import Tuple, List
import numpy as np
import pandas as pd
import random

import torch
import torchvision
from problog.logic import Term, Constant, list2term

from deepproblog.dataset import ImageDataset
from deepproblog.query import Query

Card = List[str]


class TicTacToeSeparate(ImageDataset):
    def __getitem__(self, i):
        if self.name == 'inkscape':
            return super().__getitem__("{}_{}".format(*i))
        elif self.name == 'MNIST':
            index = "{}_{}".format(*i)
            csv = pd.read_csv(self.csv, header=None)
            r, c = int(index[0]), int(index[-1])
            cell = csv.iloc[r, c]
            print(cell)
            label = self.classi.index(cell)
            y = None
            while y != label:
                img, y = random.choice(self.datasets[self.dataset])
            img = np.array(img)[0]
            if self.transform is not None:
                img = self.transform(img)
            return img
        else:
            print("[ERROR] Datasets type incorrect")
            exit(-1)

    def __len__(self):
        return len(self.data)

    def __init__(self, dataset, name, transform=None):
        if transform is None:
            transform = torchvision.transforms.Compose(
                [torchvision.transforms.ToTensor(), torchvision.transforms.Normalize((0.5,), (0.5,))]
            )
        super().__init__("data/images/{}".format(dataset), transform=transform)
        if name == 'MNIST':
            train, test = load_dataset(name, transform)
            self.datasets = {
                "train": train,
                "test": test,
            }
        self.name = name
        self.data: List[
            Tuple[Card, Card, Card, Card, Card, Card, Card, Card, Card, str, str]
        ] = []  # each example consists of the 9 cells(cards) and the result
        self.dataset = dataset
        self.csv = "data/labels/{}.csv".format(dataset)
        self.classi = ['b', 'x', 'o']
        with open(self.csv) as f:  # from csv I read the boards and the winner
            for line in f:
                cards: List[Card] = list(line.strip().split(",")) 
                winner = get_outcome(list(line.strip().split(",")))
                self.data.append(tuple(cards + [winner]))

    def to_query(self, i):
        cards = self.data[i]
        cards, outcome = cards[:9], cards[9]  # 9 cell and 1 outcome
        sub_images = [Term("card_{}".format(x)) for x in range(9)]
        images = [
            Term("tensor", Term(self.dataset, Constant(i), Constant(x)))
            for x in range(9)
        ]
        sub = {sub_images[i]: images[i] for i in range(9)}
        query = Query(Term("game", list2term(sub_images), Term(outcome)), sub)
        return query


def load_dataset(name, transform):
    if name == 'MNIST':
        train_set = torchvision.datasets.MNIST(root='./data/MNIST/', transform=transform, train=True, download=True)
        idx = torch.logical_or(torch.logical_or(train_set.targets == 1, train_set.targets == 2),
                               train_set.targets == 0)
        train_set.targets = train_set.targets[idx]
        train_set.data = train_set.data[idx]

        test_set = torchvision.datasets.MNIST(root='./data/MNIST/', transform=transform, train=False, download=True)
        idx = torch.logical_or(torch.logical_or(test_set.targets == 1, test_set.targets == 2),
                               test_set.targets == 0)
        test_set.targets = test_set.targets[idx]
        test_set.data = test_set.data[idx]
    elif name == 'inkscape':
        transform = torchvision.transforms.Compose(
            [torchvision.transforms.ToTensor(), torchvision.transforms.Normalize((0.5,), (0.5,))]
        )

        train_set = torchvision.datasets.ImageFolder(root='data/cnn/train/', transform=transform)
        test_set = torchvision.datasets.ImageFolder(root='data/cnn/test/', transform=transform)
    else:
        print("[ERROR] Datasets type incorrect")
        exit(-1)
    return train_set, test_set


def get_outcome(line):
    if win(line, 'x'):
        winner = 'x'
    elif win(line, 'o'):
        winner = 'o'
    else:
        winner = 'b'
    return winner


def win(game, player):
    idx = np.where(np.isin(game, player))[0]
    vittorie = [[0, 1, 2],
                [3, 4, 5],
                [6, 7, 8],
                [0, 3, 6], [1, 4, 7], [2, 5, 8],
                [0, 4, 8], [2, 4, 6]]
    for v in vittorie:
        if all(item in idx for item in v):
            return True
    return False
