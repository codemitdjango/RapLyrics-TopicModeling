import os
import glob
import csv


def read_txt_files(directory):
    txt_files = []
    # Suche nach allen .txt-Dateien im angegebenen Verzeichnis
    for file_path in glob.glob(os.path.join(directory, '*.txt')):
        with open(file_path, 'r', encoding='utf-8') as file:  # Hier wird die Zeichenkodierung festgelegt
            # Lese den Inhalt der Datei und füge ihn zum Array hinzu
            txt_files.append([file.read().replace("\n"," ").replace(","," ").replace('"',"").replace("'","").replace("—","").replace("-","").replace("_"," ")])
    return txt_files

def tuples_to_csv(tuples_array, output_file):
    with open(output_file, 'w', newline='', encoding='utf-8') as file:
        writer = csv.writer(file)
        for tup in tuples_array:
            writer.writerow(tup)

if __name__ == "__main__":
    directory = 'deutsch'
    txt_contents = read_txt_files(directory)
    txt_contents.insert(0,["lyric"])

    tuples_to_csv(txt_contents, "lyrics-german-new.csv")