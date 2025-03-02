import requests
import csv
import os

from dotenv import load_dotenv

load_dotenv()

# edit the event code (2025vagle, 2025mdsev, 2025chcmp, etc...)
EVENT_CODE = "2025vagle"
URL = f"https://www.thebluealliance.com/api/v3/event/{EVENT_CODE}/matches"

# put the directory here
CSV_FILE_LOCATION = f"tba_scripts/{EVENT_CODE}_event_schedule.csv"

res = requests.get(URL, headers={"X-TBA-Auth-Key": os.getenv("AUTH_KEY")})
print(f"Response Code: {res.status_code}")

parsed_matches = []


res = res.json()
for match in res:
    match_info = {}
    match_info["match_number"] = match["match_number"]

    # red alliance
    [red1, red2, red3] = match["alliances"]["red"]["team_keys"]
    match_info["R1"] = red1[3:]
    match_info["R2"] = red2[3:]
    match_info["R3"] = red3[3:]

    # red alliance
    [blue1, blue2, blue3] = match["alliances"]["blue"]["team_keys"]
    match_info["B1"] = blue1[3:]
    match_info["B2"] = blue2[3:]
    match_info["B3"] = blue3[3:]
    parsed_matches.append(match_info)

parsed_matches = sorted(parsed_matches, key=lambda x: x["match_number"])

with open(CSV_FILE_LOCATION, "w+", newline="") as csv_file:
    writer = csv.DictWriter(csv_file, fieldnames=["match_number", "R1", "R2", "R3", "B1", "B2", "B3"])
    # to write headers
    writer.writeheader()
    writer.writerows(parsed_matches)

    print(f"Wrote To: {CSV_FILE_LOCATION}")