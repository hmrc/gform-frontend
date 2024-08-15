interface ColorPickerProps {
  onChange: (color: string) => void;
}

export const Colors = {
  Yellow: "#FBF290",
  Orange: "#F5C372",
  Green: "#B8EA9C",
  Blue: "#ACD9F8",
  Purple: "#D6C7FB",
  Red: "#F3B9D2",
} as const;

type NoteColor =
  | typeof Colors.Yellow
  | typeof Colors.Orange
  | typeof Colors.Green
  | typeof Colors.Blue
  | typeof Colors.Purple
  | typeof Colors.Red;

export const ColorPicker = ({ onChange }: ColorPickerProps) => {
  const handleColorChange = (event: Event) => {
    const target = event.target as HTMLButtonElement;
    const color = target.style.backgroundColor;

    if (onChange) {
      onChange(color);
    }
  };

  const buttons = Object.values(Colors).map((color) => (
    <button
      class="color-btn"
      style={{
        backgroundColor: color,
      }}
      name="yellow"
      onClick={handleColorChange}
    />
  ));

  return <div class="note-controls">{buttons}</div>;
};

export default ColorPicker;
