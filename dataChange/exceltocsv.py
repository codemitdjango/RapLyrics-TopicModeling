import csv

def readFile(path: str) -> str:
    tuples = []
    with open(path, 'r', newline='', encoding='utf-8') as file:
        reader = csv.reader(file)
        header = next(reader)  # Read the header
        for row in reader:
            tuples.append(tuple(row))
    return tuples

def transform_tuple_array(tuples_array: [()]) -> str:
    return [transform_tuple(t) for t in tuples_array]

def transform_tuple(tuple: ()) -> ():
    return (tuple[1],tuple[3])

def tuples_to_csv(tuples_array, output_file):
    with open(output_file, 'w', newline='', encoding='utf-8') as file:
        writer = csv.writer(file)
        for tup in tuples_array:
            writer.writerow(tup)

if __name__ == "__main__":
    tuples_array = readFile('baseform_german_web_2012.csv')
    transformed_array = transform_tuple_array(tuples_array)
    transformed_array.insert(0, ("inflected_form", "lemma"))
    tuples_to_csv(transformed_array, "baseform-german-transformed")
