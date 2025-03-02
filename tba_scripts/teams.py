import requests
import csv
import os

from dotenv import load_dotenv

load_dotenv()

# edit the event code (2025vagle, 2025mdsev, 2025chcmp, etc...)
EVENT_CODE = "2025vagle"
URL = f"https://www.thebluealliance.com/api/v3/event/{EVENT_CODE}/teams"

# put the directory here
CSV_FILE_LOCATION = f"tba_scripts/{EVENT_CODE}_teams.csv"

res = requests.get(URL, headers={"X-TBA-Auth-Key": os.getenv("AUTH_KEY")})
print(f"Response Code: {res.status_code}")

parsed_teams = []

res = res.json()
for team in res:
    parsed_teams.append(team["key"][3:])

parsed_teams = sorted(parsed_teams, key=lambda x: int(x))

with open(CSV_FILE_LOCATION, "w", newline="") as csv_file:
    writer = csv.writer(csv_file)
    writer.writerow(["teams"])
    for team in parsed_teams:
        writer.writerow([team])

    print(f"Wrote To: {CSV_FILE_LOCATION}")