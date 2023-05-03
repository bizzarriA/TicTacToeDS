import argparse
import random
import subprocess
from pathlib import Path

import numpy as np
from PIL import Image


parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)

parser.add_argument('--size', type=int, default=28, help='img size (default 28)')
parser.add_argument('--pretrain', type=bool, default=True, help='create dataset for pretrain phase')
opt = parser.parse_args()


values = ["x", "o", "b"]
IMG_H = opt.size
pretrained = opt.pretrain


def render_svg(in_path, out_path, height):
    print(f"Rendering {in_path} to {out_path}")
    Path(out_path).parent.mkdir(parents=True, exist_ok=True)
    subprocess.run(
        [
            "inkscape",
            in_path,
            "-h",
            str(height),
            "--export-filename=" + out_path,
            "-b",
            '"#FFFFFF"',
        ]
    )


def get_images(img_h):
    images = {}
    for value in values:
        path = Path("cards/rendered/{1}/{0}.png".format(value, img_h))
        if not path.is_file():
            render_svg(
                "cards/{}.svg".format(value),
                "cards/rendered/{1}/{0}.png".format(value, img_h),
                img_h,
            )
        images[value] = Image.open(path)
    return images


def render_all(games, img_h, path):
    imgs = get_images(img_h)
    path.mkdir(parents=True, exist_ok=True)
    for i, cards in enumerate(games):
        images = [apply_transformations(imgs[card]) for card in cards]
        for j, img in enumerate(images):
            img.save(path / "{}_{}.png".format(i, j))


def render_for_class(num, img_h, dir):
    imgs = get_images(img_h)
    path = Path(dir)
    path.mkdir(parents=True, exist_ok=True)
    for c in values:
        class_dir = dir + c
        class_path = Path(class_dir)
        class_path.mkdir(parents=True, exist_ok=True)
    for i in range(num):
        images = [apply_transformations(imgs[card]) for card in values]
        for j, img in enumerate(images):
            img.save(path / "{}/{}.png".format(values[j], i))


def apply_transformations(image, max_rot=10, hue=13, sat=13, val=13):
    image = image.convert("HSV")
    data = np.array(image, dtype="int16")

    for i, v in enumerate([hue, sat, val]):
        data[:, :, i] += np.random.randint(-v, v, dtype="int16")
        data[:, :, 0] = data[:, :, 0] % 255
        data = data.clip(0, 255)
    image = Image.fromarray(data.astype("uint8"), mode="HSV")
    image = image.convert("RGB")

    w, h = image.width, image.height
    theta = random.uniform(-max_rot, max_rot)
    image = image.rotate(
        theta, resample=Image.BILINEAR, expand=True, fillcolor="#FFFFFF"
    )
    image = image.resize((w, h))

    return image


def read_file(path):
    games = []
    with open(path) as f:
        for line in f:
            cards = line.strip().split(",")[:9]
            cards = list(map(lambda x: x.replace('"', ''), cards))
            games.append(cards)
    return games


for dataset in ["train", "test"]:
    render_all(
        read_file("labels/{}.csv".format(dataset)),
        IMG_H,
        Path("images/{}/".format(dataset)),
    )
if pretrained:
    render_for_class(
        1000, IMG_H, "cnn/"
    )
