#!/bin/bash

CREDENTIALS_FILE=../credentials.csv

if [ -z "$1" ]; then
  echo "Usage: $0 <number_of_players>"
  echo "Generates <number_of_players> Playwright spec files based on ${CREDENTIALS_FILE}"
  exit 1
fi

if ! [[ "$1" =~ ^[0-9]+$ ]] || [ "$1" -le 0 ]; then # Ensure it's a positive integer
  echo "Error: Number of players must be a positive integer."
  exit 1
fi
NUM_PLAYERS_TO_GENERATE=$1

# --- File Checks ---
if [ ! -f "$CREDENTIALS_FILE" ]; then
  echo "Error: Credentials file '${CREDENTIALS_FILE}' not found in the current directory."
  exit 1
fi

# --- Main Logic ---
echo "Generating ${NUM_PLAYERS_TO_GENERATE} spec file(s)..."

player_count=0
# Read the CSV file, skip the header (NR>1 for awk, or tail -n +2 for shell)
# Using tail and a while loop for better control over shell variable expansion
tail -n +2 "$CREDENTIALS_FILE" | while IFS=, read -r username_raw password_raw || [[ -n "$username_raw" ]]; do
  if [ "$player_count" -ge "$NUM_PLAYERS_TO_GENERATE" ]; then
    break
  fi

  # Trim potential carriage returns, common if CSV is from Windows
  username=$(echo "$username_raw" | tr -d \'\\r\')
  password_val=$(echo "$password_raw" | tr -d \'\\r\')

  if [ -z "$username" ] || [ -z "$password_val" ]; then
    echo "Warning: Skipping line with empty username or password (original user: \'${username_raw}\')."
    continue
  fi

  # Make username and password safe for JavaScript string literals by escaping single quotes
  username_js_safe=$(echo "$username" | sed "s/'/\\\\\\\\'\'\'/g")
  password_js_safe=$(echo "$password_val" | sed "s/'/\\\\\\\\'\'\'/g")

  FILENAME="ctfg-${username}.spec.ts"
  # If using OUTPUT_DIR: FILENAME="${OUTPUT_DIR}/ctfg-${username}.spec.ts"
  echo "Creating ${FILENAME} for user ${username}..."

  cat > "$FILENAME" << EOF
import { test, expect } from '@playwright/test';

const CARD_SELECTOR   = 'div.cursor-pointer';      // the card <div>

test('ctfg log in', async ({ page }) => {

  const username = '${username_js_safe}';
  const password = '${password_js_safe}';

  await page.goto('http://localhost:8080');

  // These inputs have no <label>, so use the placeholder text
  await page.getByPlaceholder('Username').fill(username);
  await page.getByPlaceholder('Password').fill(password);

  // The button’s accessible name is “Login”
  page.getByRole('button', { name: /^login$/i }).click();

  // These inputs have no <label>, so use the placeholder text
  await page.getByPlaceholder('e.g. CryptoCats').fill('display_'+username);

  // The button’s accessible name is “Save & Continue”
  await page.getByRole('button', { name: /^save & continue$/i }).click();

  /* ------------------------------------------------------------------
   *  Post-login assertion
   * ------------------------------------------------------------------ */

  // Your client code shows the first challenge heading is “Cryptography”
  await expect(
    page.getByRole('heading', { name: /cryptography/i })
  ).toBeVisible();

  const cards = page.locator(CARD_SELECTOR);
  const total = await cards.count();

  for (let i = 0; i < total; i++) {
    const card = cards.nth(i);

    await card.scrollIntoViewIfNeeded();
    await card.click();

    await page.getByPlaceholder('CTF{...}').fill('foo');
    await page.getByRole('button', { name: /^submit$/i }).click();

    await expect(page.getByText('Challenge Solved!', { exact: true }))
      .toBeVisible();

   await page.locator('#back-btn').click();
  }

  await page.getByRole('button', { name: 'Scoreboard' }).click();

  await page.waitForTimeout(4000);

  await page.screenshot({ path: 'home-full.png', fullPage: true });
});
EOF

  if [ $? -ne 0 ]; then
    echo "Error: Failed to create ${FILENAME}."
  fi

  player_count=$((player_count + 1))
done
