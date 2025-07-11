/* playwright.config.ts

   SPDX-License-Identifier: MIT

   Copyright (C) 2025 Anthony Green */

import { defineConfig, devices } from '@playwright/test';

export default defineConfig({
  use: { baseURL: 'http://localhost:8080',
         trace: 'on-first-retry' },

  webServer: {
    command: 'rm -rf events.db* && ./ctfg -d',
    url:     'http://localhost:8080',
    stdout:  'pipe',
    stderr:  'pipe',
    reuseExistingServer: !process.env.CI,
    timeout: 120_000
  },

  projects: [
    { name: 'chromium', use: { ...devices['Desktop Chrome'] } },
  ],

  testDir: './tests',
  fullyParallel: true,
  retries: process.env.CI ? 2 : 0,
  workers: process.env.CI ? 10 : undefined,
  reporter: [
    ['html', { outputFolder: 'playwright-report', open: 'never' }],
    ['list']
  ],
});
