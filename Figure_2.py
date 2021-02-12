from PIL import Image
from PIL import ImageDraw
from PIL import ImageFont
import os
import tkinter as tk
from tkinter import filedialog


def select_directory(prompt_text):
    root = tk.Tk()
    root.withdraw()
    return filedialog.askdirectory(title=prompt_text)


def select_file(prompt_text):
    root = tk.Tk()
    root.withdraw()
    return filedialog.askopenfilename(title=prompt_text)


def extract_raw_img_and_segmentation(preview_img, position):
    if preview_img.size == (1024, 2560):
        raw_scan = preview_img.crop((0, (position + 1) * 512, 512, (position + 2) * 512))
        segmentation = preview_img.crop((513, (position + 1) * 512, 1024, (position + 2) * 512))
        return raw_scan, segmentation


def remove_VAT_segmentation(preview_img):
    pixels = preview_img.load()
    height, width = preview_img.size
    for h in range(height):
        for w in range(width):
            r, g, b = pixels[h, w]
            if r==255 and g==255 and b==0:  # check if this is 100% yellow (VAT)
                pixels[h, w] = 0, 0, 0  # set to black
    return preview_img


def remove_SAT_segmentation(preview_img):
    pixels = preview_img.load()
    height, width = preview_img.size
    for h in range(height):
        for w in range(width):
            r, g, b = pixels[h, w]
            if r==0 and g==128 and b==0:  # check if this is 50% green (SAT)
                pixels[h, w] = 0, 0, 0  # set to black
    return preview_img


def create_segmentation_2by4(preview_img_path, level_order):
    bg = Image.new('RGB', (1024, 2048))
    segs = Image.open(preview_img_path)

    levels = ['T5', 'T8', 'T10', 'L3']
    for level in levels:
        pos = level_order.index(level)
        raw_img, seg = extract_raw_img_and_segmentation(segs, pos)
        bg.paste(raw_img, (0, 512 * levels.index(level)))
        bg.paste(seg, (512, 512 * levels.index(level)))

    return bg


def two_x_n_panel_to_custom_aspect_ratio(img, aspect_ratio=(1, 2)):
    dim = img.size

    if aspect_ratio[0] / aspect_ratio[1] <= dim[0] / dim[1]:  # cropping the y-axis
        letterbox_height = (dim[1] - dim[0] * aspect_ratio[1] / aspect_ratio[0]) // 4
        upper_half = img.crop((0, letterbox_height, dim[0], dim[1] // 2 - letterbox_height))
        lower_half = img.crop((0, dim[1] // 2 + letterbox_height, dim[0], dim[1] - letterbox_height))
        bg = Image.new("RGB", (dim[0], dim[0] * aspect_ratio[1] // aspect_ratio[0]))
        bg.paste(upper_half, (0, 0))
        bg.paste(lower_half, (0, bg.size[1] // 2))

        return bg

    else:
        letterbox_width = (dim[0] - dim[1] * aspect_ratio[0] / aspect_ratio[1]) // 4
        left_half = img.crop((letterbox_width, 0, dim[0] // 2 - letterbox_width, dim[1]))
        right_half = img.crop((dim[0] // 2 + letterbox_width, 0, dim[0] - letterbox_width, dim[1]))
        bg = Image.new("RGB", (dim[0], dim[0] * aspect_ratio[1] // aspect_ratio[0]))
        bg.paste(left_half, (0, 0))
        bg.paste(right_half, (bg.size[0] // 2, 0))

        return bg


def crop_n_x_n_panel(img, num_panel_rows, num_panel_columns, crop_x=0, crop_y=0):
    dim = img.size
    panel_x_dim = dim[0] // num_panel_columns
    panel_y_dim = dim[1] // num_panel_rows
    panel_x_crop = (panel_x_dim * crop_x) // 2
    panel_y_crop = (panel_y_dim * crop_y) // 2
    composite_x_dim = int(dim[0] * (1 - crop_x))
    composite_y_dim = int(dim[1] * (1 - crop_y))
    composite = Image.new('RGB', (composite_x_dim, composite_y_dim))

    for row in range(num_panel_rows):
        for col in range(num_panel_columns):
            cropped_panel = img.crop((col * panel_x_dim + panel_x_crop, row * panel_y_dim + panel_y_crop,
                                      (col + 1) * panel_x_dim - panel_x_crop, (row + 1) * panel_y_dim - panel_y_crop))
            composite.paste(cropped_panel, (col * (composite_x_dim // num_panel_columns),
                                            row * (composite_y_dim // num_panel_rows)))

    return composite


def two_x_n_to_transparent_overlay(img, overlay_opacity):
    dim = img.size
    volumes = img.crop((0, 0, dim[0] // 2, dim[1]))
    segmentations = img.crop((dim[0] // 2, 0, dim[0], dim[1]))
    segmentations.putalpha(overlay_opacity)
    volumes.putalpha(255)

    volumes.alpha_composite(segmentations)

    return volumes


def transparent_1x4_to_2x2(img):
    dim = img.size
    bg = Image.new('RGB', (dim[0] * 2, dim[1] // 2))

    img_1 = img.crop((0, 0, dim[0], dim[1] // 4))
    img_2 = img.crop((0, dim[1] // 4, dim[0], dim[1] // 2))
    img_3 = img.crop((0, dim[1] // 2, dim[0], dim[1] // 4 * 3))
    img_4 = img.crop((0, dim[1] // 4 * 3, dim[0], dim[1]))

    bg.paste(img_1, (0, 0))
    bg.paste(img_2, (bg.size[0] // 2, 0))
    bg.paste(img_3, (0, bg.size[1] // 2))
    bg.paste(img_4, (bg.size[0] // 2, bg.size[1] // 2))

    return bg


def label_panel_image(img, num_panel_rows, num_panel_columns, offset=(20, 20), labels='ABCDEFGHIJKLMNOPQRSTUVWXYZ'):
    dim = img.size
    panel_x_dim = dim[0] / num_panel_columns
    panel_y_dim = dim[1] / num_panel_rows
    fonts_folder = r"C:\WINDOWS\fonts"
    font_size = int(panel_x_dim // 6)
    calibri_font = ImageFont.truetype(os.path.join(fonts_folder, 'calibri.ttf'), font_size)

    img_draw = ImageDraw.Draw(img)
    label_index = 0
    for row in range(num_panel_rows):
        for col in range(num_panel_columns):
            img_draw.text((int(col * panel_x_dim) + offset[0], int(row * panel_y_dim) + offset[1]), labels[label_index],
                          fill="white", font=calibri_font)
            label_index += 1

    return img


def main():
    preview_path = select_file("Select the preview image")
    fp = select_directory("Where should I store the output graphic?") + "\\" + input("What is the output filename?")
    level_order_string = input(
        "Type in the names of all used levels in top-to-bottom order, separated by a semicolon (\";\"):")
    create_transparent_overlay = input("Composite into transparent overlay? Enter 'Y' for yes, any key for no.") == 'Y'
    level_order = level_order_string.split(";")
    crop_x = float(input("By how much do you want to crop the x-axis (range 0-1?"))
    crop_y = float(input("By how much do you want to crop the y-axis (range 0-1?"))

    out_img = crop_n_x_n_panel(create_segmentation_2by4(preview_path, level_order), 4, 2, crop_x, crop_y)
    if create_transparent_overlay:
        alpha = int(input('What opacity should the overlay have (range 0-255)?'))
        out_img = two_x_n_to_transparent_overlay(out_img, alpha)
        out_img = label_panel_image(transparent_1x4_to_2x2(out_img), 4, 1)

    out_img.save(fp)


def create_figure_two():  # a variation on the main graph with my specific ipro settings
    preview_path = select_file("Select the preview image")
    fp = select_directory("Where should I store the output graphic?") + "\\" + input("What is the output filename?")
    level_order = ['T8', 'T10', 'T5', 'L3']

    out_img = crop_n_x_n_panel(create_segmentation_2by4(preview_path, level_order), 4, 2, 0.1, 0.4)
    out_img = remove_VAT_segmentation(out_img)
    out_img = remove_SAT_segmentation(out_img)
    out_img = two_x_n_to_transparent_overlay(out_img, 51)

    out_img = label_panel_image(out_img, 4, 1)
    out_img.save(fp)


if __name__ == "__main__":
    main()
