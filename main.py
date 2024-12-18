def scale_sprite(sprite, width, height, scale_factor=2):
    scaled_sprite = []
    for row in range(height):
        original_row = sprite[row * width:(row + 1) * width]
        scaled_row = []
        # Scale the row horizontally
        for pixel in original_row:
            scaled_row.extend([pixel] * scale_factor)
        # Add the scaled row multiple times for vertical scaling
        for _ in range(scale_factor):
            scaled_sprite.extend(scaled_row)
    return scaled_sprite, width * scale_factor, height * scale_factor


def print_scaled_sprite(scaled_sprite, new_width):
    print(f"_rook_black   dw {new_width}, {len(scaled_sprite) // new_width}")
    for i in range(0, len(scaled_sprite), new_width):
        row = scaled_sprite[i:i + new_width]
        print("                      db " + ", ".join(map(str, row)))


# Original sprite data (as a flat list)
king_black = [
    9, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 9,
    9, 0, 3, 0, 3, 0, 3, 3, 3, 3, 0, 9,
    9, 0, 3, 0, 0, 0, 3, 3, 3, 3, 0, 9,
    9, 0, 3, 3, 3, 3, 3, 3, 3, 3, 0, 9,
    9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9,
    9, 9, 0, 3, 3, 3, 3, 3, 3, 0, 9, 9,
    9, 9, 9, 0, 0, 0, 0, 0, 0, 9, 9, 9,
    9, 9, 9, 0, 3, 3, 3, 3, 0, 9, 9, 9,
    9, 9, 9, 0, 3, 3, 3, 3, 0, 9, 9, 9,
    9, 9, 9, 0, 3, 3, 3, 3, 0, 9, 9, 9,
    9, 9, 0, 3, 3, 3, 3, 3, 3, 0, 9, 9,
    9, 0, 3, 3, 3, 3, 3, 3, 3, 3, 0, 9,
    0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
    0, 0, 0, 0, 0, 0, 0 ,0, 0, 0, 0, 0
]
width = 12
height = 14

# Scale the sprite
scaled_sprite, new_width, new_height = scale_sprite(king_black, width, height, scale_factor=2)

# Print the scaled sprite
print_scaled_sprite(scaled_sprite, new_width)
