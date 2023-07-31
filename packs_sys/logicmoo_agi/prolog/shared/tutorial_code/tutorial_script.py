import json

import os
import numpy as np


# Define function to read tasks
def load_tasks(path):
    """
    Function to load .json files of tasks
    :param path: Path to folder where tasks are stored
    :return: - training and test tasks separated into a list of dictionaries
                    where each entry is of the type {'input': [.task.], 'output': [.task.]}
             - list of file names
    """
    # Load Tasks
    # Path to tasks 
    tasks_path = path
    # Initialize list to s
    # tore file names of tasks
    tasks_file_names = list(np.zeros(len(os.listdir(tasks_path))))
    # Initialize lists of lists of dictionaries to store training and test tasks
    # Format of items will be [{'input': array,'output': array},...,{'input': array,'output': array}]
    tasks_count = len(os.listdir(tasks_path))
    train_tasks = list(np.zeros(tasks_count))
    test_tasks = list(np.zeros(tasks_count))

    # Read in tasks and store them in lists initialized above
    for i, file in enumerate(os.listdir(tasks_path)):
        with open(tasks_path + file, 'r') as f:
            task = json.load(f)
            tasks_file_names[i] = file
            train_tasks[i] = []
            test_tasks[i] = []

            for t in task['train']:
                train_tasks[i].append(t)
            for t in task['test']:
                test_tasks[i].append(t)

    return train_tasks, test_tasks, tasks_file_names


# Read in evaluation tasks
training_tasks, testing_tasks, file_names = load_tasks('data/evaluation/')
# Get number of test tasks for outputting progress later and define counter.
num_test_tasks = len(testing_tasks)
counter = 0
# Do some stuff to generate solution
# Allocate space for overall solution
solution = []
# Iterate over all tasks to generate solution
for test_task, task_filename in zip(testing_tasks, file_names):
    # Allocate space for solutions of task examples
    test = []
    # Store filename
    task_name = task_filename.strip('.json')
    # Iterate over test examples (1 or 2)
    for id_example, example in enumerate(test_task):
        # Get input of example
        example_input = example['input']
        # Do some stuff to generate output
        # Get maximal value of input
        input_max = np.amax(example_input)
        # Do some random stuff to generate outputs
        # Random grid sizes
        random_grid_sizes = np.random.randint(1, 5, size=(3, 2))
        # Allocate space for predictiction objects 
        predictions = []
        # Make predictions taking random grid sizes and filling the resulting arrays with color found above
        for prediction_id, grid_size in enumerate(random_grid_sizes):
            # Generate output prediction and change it to a list to create json file later
            output = np.full(grid_size, input_max, dtype=np.uint8).tolist()
            object_prediction = {'prediction_id': prediction_id, 'output': output}
            predictions.append(object_prediction)
        # Generate object solution containing all predictions
        object_solution = {'output_id': id_example, 'number_of_predictions': len(predictions),
                           'predictions': predictions}
        # Add solution of example to list of solutions
        test.append(object_solution)
    # Add solution of examples to overall solution
    object_task = {'task_name': task_name, 'test': test}
    solution.append(object_task)
    # Output progress
    counter += 1
    if counter % 50 == 0:
        print('Generated solution for {} of {} test examples'.format(counter, num_test_tasks))

# Store solution to json file named solution_teamid where our teamid is lab42
# Store it in solution folder which is mounted
solution_json = json.dumps(solution)
with open('../data/solution/solution_logicmoo.json', 'w') as outfile:
    outfile.write(solution_json)
# Print that program has finished
print("Program has finished!")
