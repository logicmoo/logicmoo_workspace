from gym_sokoban.envs.sokoban_env_fast import SokobanEnvFast
import pandas as pd

def check_solution(actions, seed=0, size=8, num_boxes=2):
    dim_room = (size, size)

    env = SokobanEnvFast(dim_room=dim_room,
                         num_boxes=num_boxes,
                         seed=seed,
                         penalty_for_step=0)

    env.reset()

    rewards = []
    for action in actions:
        observation, reward, done, info = env.step(action)
        rewards.append(reward)
    if rewards[-1] >= 10:
        return 1
    else:
        return 0


if __name__ == "__main__":

    df = pd.read_csv('results.csv')

    results = 0
    for index, row in df.iterrows():
        print("Current trial {} seed {} actions {}".format(index, row.seed, row.actions))
        if len(row.actions) > 2:
            actions = row.actions.strip('][').split(', ')
            actions = [int(action) for action in actions]
            results += check_solution(actions, row.seed, size=8, num_boxes=2)

    print("Number of solutions: {}".format(results))
    if results >= 50:
        print("Passed!")
