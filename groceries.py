import argparse

parser = argparse.ArgumentParser()
parser.add_argument("TOTAL", help="total groceries cost", type=float)
parser.add_argument("NONVEGAN", help="non-vegan cost", type=float)

args = parser.parse_args()

TOTAL = args.TOTAL
NONVEGAN = args.NONVEGAN

vegan_cost_per_5 = (TOTAL - NONVEGAN) / 5
nonvegan_cost_per_4 = NONVEGAN / 4

print("Vegan person: ", vegan_cost_per_5)
print("Non-vegan person: ", (vegan_cost_per_5 + nonvegan_cost_per_4))
