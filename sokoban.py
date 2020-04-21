from pyswip import Prolog
from gym_sokoban.envs.sokoban_env_fast import SokobanEnvFast
import time

# example_query = "call_with_time_limit(5,solve([[top(x1y1,x1y2),top(x1y2,x1y3),top(x1y3,x1y4),top(x1y4,x1y5),top(x1y5,x1y6),top(x2y1,x2y2),top(x2y2,x2y3),top(x2y3,x2y4),top(x2y4,x2y5),top(x2y5,x2y6)],[right(x1y1,x2y1),right(x1y2,x2y2),right(x1y3,x2y3),right(x1y4,x2y4),right(x1y5,x2y5),right(x1y6,x2y6),right(x2y6,x3y6)],[box(x1y4),box(x2y6)],[solution(x1y2),solution(x1y6)],sokoban(x3y6)],Solution))"

def flatten(container):
    for i in container:
        if isinstance(i, (list,tuple)):
            for j in flatten(i):
                yield j
        else:
            yield i


def map_moves(move):
    if move == "up":
        return 3
    elif move == "down":
        return 2
    elif move == "left":
        return 0
    elif move == "right":
        return 1

def find_solution(size=8, num_boxes=2, time_limit=10, seed=0):
    dim_room = (size, size)

    env = SokobanEnvFast(dim_room=dim_room,
                         num_boxes=num_boxes,
                         seed=seed,
                         penalty_for_step=0)
    # The encoding of the board is described in README
    board = env.reset()

    wall = board[:,:,0] # this is a one-hot encoding of walls
    # For readibility first we deal with tops and then with rights
    tops = []
    for i in range(dim_room[0]):
        for j in range(dim_room[1]-1):
            if wall[i,j] == 0 and wall[i,j+1] == 0:
                tops.append("top(x{}y{},x{}y{})".format(i,j,i,j+1))

    rights = []
    for i in range(dim_room[0]-1):
        for j in range(dim_room[1]):
            if wall[i,j] == 0 and wall[i+1,j] == 0:
                rights.append("right(x{}y{},x{}y{})".format(i,j,i+1,j))

    boxes_initial_locations = board[:,:,4]
    boxes_initial = []
    for i in range(dim_room[0]):
        for j in range(dim_room[1]):
            if boxes_initial_locations[i,j] == 1:
                boxes_initial.append("box(x{}y{})".format(i,j))

    boxes_target_locations = board[:,:,2]
    boxes_target = []
    for i in range(dim_room[0]):
        for j in range(dim_room[1]):
            if boxes_target_locations[i,j] == 1:
                boxes_target.append("solution(x{}y{})".format(i,j))

    sokoban_initial_location = board[:,:,5] + board[:,:,6]
    for i in range(dim_room[0]):
        for j in range(dim_room[1]):
            if sokoban_initial_location[i,j] == 1:
                sokoban_string = "sokoban(x{}y{})".format(i,j)
                break

    tops_string = "[" + ','.join(tops) + ']'
    rights_string = "[" + ','.join(rights) + ']'
    boxes_initial_string = "[" + ','.join(boxes_initial) + ']'
    boxes_target_string = "[" + ','.join(boxes_target) + ']'

    prolog = Prolog()
    prolog.consult("sokoban.pl")
    query = "call_with_time_limit({},solve([{},{},{},{},{}],Solution))".format(time_limit,
                                                                               tops_string,
                                                                               rights_string,
                                                                               boxes_initial_string,
                                                                               boxes_target_string,
                                                                               sokoban_string)

    print(query)
    try:
        result = list(prolog.query(query))
        rewards = []
        for i, r in enumerate(result):
            solution = r['Solution']
            actions = []
            for index in range(len(solution)):
                move = str(solution[index]).split()[-1]
                move = move[:-1]
                action = map_moves(move)
                actions.append(action)
                observation, reward, done, info = env.step(action)
                rewards.append(reward)
            print("Last return {}".format(rewards[-1]))
            if rewards[-1] >= 10:
                return 1
            else:
                return 0
    except:
        return 0


if __name__ == "__main__":

    number_of_trials = 100
    time_start = time.time()

    results = 0
    for seed in range(number_of_trials):
        print("Current trial {} result {}".format(seed, results))
        results += find_solution(size=8, num_boxes=2, time_limit=20, seed=seed)

    print("Number of solutions: {}".format(results))
    print("Total time: {}".format(time.time() - time_start))
