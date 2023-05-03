from TicTacToe import TicTacToeSeparate
import argparse
import numpy as np
import os
from matplotlib import pyplot as plt

parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)

parser.add_argument('--dataset', default='MNIST', help='name dataset (MNIST or Inkscape - MNIST default)')

opt = parser.parse_args()

ds_names = ['MNIST', 'inkscape']

if opt.dataset == 'inkscape' and not os.path.exists("data/images/train/"):
    print("Please before use inkscape dataset, run render.py file in data dir")
    exit(1)
if opt.dataset not in ds_names:
    print("Dataset must be in: ", ds_names)
    exit(1)
dataset_name = opt.dataset
datasets = {
    "train": TicTacToeSeparate("train", dataset_name),
    "test": TicTacToeSeparate("test", dataset_name)
}

plt.figure(figsize=(6, 6))

for i in range(9):
    plt.subplot(3, 3, i + 1)
    img = datasets['test'].__getitem__((0, i))
    img = np.transpose(img, (1, 2, 0))
    plt.imshow(img)
plt.show()
