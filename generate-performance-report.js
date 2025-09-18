#!/usr/bin/env node

const fs = require('fs');

function generatePerformanceReport(metricsData, outputFile = 'performance-report.html') {
    const {
        startTime,
        endTime,
        players,
        totals,
        timeline,
        responseTimes
    } = metricsData;

    const duration = endTime - startTime;
    const durationSeconds = Math.round(duration / 1000);

    // Calculate statistics
    const stats = {
        duration: durationSeconds,
        totalPlayers: players.length,
        successfulLogins: totals.loginSuccess,
        loginSuccessRate: (totals.loginSuccess / totals.loginAttempts * 100).toFixed(1),
        wsSuccessRate: (totals.wsSuccessful / totals.wsConnections * 100).toFixed(1),
        flagSuccessRate: (totals.flagsCorrect / totals.flagSubmissions * 100).toFixed(1),
        averageResponseTimes: {
            login: calculateAverage(responseTimes.login),
            submit: calculateAverage(responseTimes.submit),
            challenges: calculateAverage(responseTimes.challenges),
            staticFiles: calculateAverage(responseTimes.staticFiles)
        },
        percentiles: {
            login: calculatePercentiles(responseTimes.login),
            submit: calculatePercentiles(responseTimes.submit)
        },
        staticFileBreakdown: metricsData.staticFileBreakdown ? {
            'index.html': calculatePercentiles(metricsData.staticFileBreakdown['index.html'] || []),
            'CSS': calculatePercentiles(metricsData.staticFileBreakdown['CSS'] || []),
            'banner image': calculatePercentiles(metricsData.staticFileBreakdown['banner image'] || []),
            'app.js': calculatePercentiles(metricsData.staticFileBreakdown['app.js'] || [])
        } : null,
        throughput: {
            loginsPerSecond: (totals.loginSuccess / durationSeconds).toFixed(2),
            flagsPerSecond: (totals.flagsCorrect / durationSeconds).toFixed(2)
        },
        userExperience: calculateUserExperienceMetrics(players, startTime)
    };

    // Error analysis
    const errorsByType = {};
    totals.errors.forEach(error => {
        errorsByType[error.context] = (errorsByType[error.context] || 0) + 1;
    });

    // Timeline data for charts
    const timelineData = generateTimelineData(timeline, startTime, endTime);

    const html = `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>CTFG Performance Test Report</title>
    <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>
    <style>
        body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
            margin: 0;
            padding: 20px;
            background: #f5f5f5;
        }
        .container {
            max-width: 1200px;
            margin: 0 auto;
            background: white;
            border-radius: 8px;
            box-shadow: 0 2px 10px rgba(0,0,0,0.1);
            overflow: hidden;
        }
        .header {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 30px;
            text-align: center;
        }
        .header h1 {
            margin: 0;
            font-size: 2.5em;
        }
        .subtitle {
            margin: 10px 0 0 0;
            opacity: 0.9;
        }
        .content {
            padding: 30px;
        }
        .metrics-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
            gap: 20px;
            margin-bottom: 40px;
        }
        .metric-card {
            background: #f8f9fa;
            border-radius: 8px;
            padding: 20px;
            text-align: center;
            border-left: 4px solid #667eea;
        }
        .metric-value {
            font-size: 2em;
            font-weight: bold;
            color: #333;
            margin-bottom: 5px;
        }
        .metric-label {
            color: #666;
            font-size: 0.9em;
        }
        .chart-container {
            margin: 30px 0;
            padding: 20px;
            background: #f8f9fa;
            border-radius: 8px;
        }
        .chart-title {
            font-size: 1.3em;
            font-weight: bold;
            margin-bottom: 15px;
            color: #333;
        }
        .chart-wrapper {
            position: relative;
            height: 400px;
            width: 100%;
        }
        .error-section {
            margin-top: 30px;
            padding: 20px;
            background: #fff5f5;
            border-radius: 8px;
            border-left: 4px solid #ef4444;
        }
        .error-title {
            font-size: 1.2em;
            font-weight: bold;
            color: #dc2626;
            margin-bottom: 15px;
        }
        .error-list {
            list-style: none;
            padding: 0;
        }
        .error-item {
            padding: 8px 0;
            border-bottom: 1px solid #fee2e2;
        }
        .timestamp {
            color: #666;
            font-family: monospace;
            font-size: 0.9em;
        }
        table {
            width: 100%;
            border-collapse: collapse;
            margin: 20px 0;
        }
        th, td {
            padding: 12px;
            text-align: left;
            border-bottom: 1px solid #ddd;
        }
        th {
            background: #f8f9fa;
            font-weight: bold;
        }
        .success { color: #059669; }
        .warning { color: #d97706; }
        .error { color: #dc2626; }
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>🚀 CTFG Performance Test Report</h1>
            <div class="subtitle">Generated on ${new Date().toLocaleString()}</div>
        </div>

        <div class="content">
            <!-- Key Metrics -->
            <div class="metrics-grid">
                <div class="metric-card">
                    <div class="metric-value">${stats.totalPlayers}</div>
                    <div class="metric-label">Total Players</div>
                </div>
                <div class="metric-card">
                    <div class="metric-value">${stats.duration}s</div>
                    <div class="metric-label">Test Duration</div>
                </div>
                <div class="metric-card">
                    <div class="metric-value ${stats.loginSuccessRate >= 95 ? 'success' : stats.loginSuccessRate >= 80 ? 'warning' : 'error'}">${stats.loginSuccessRate}%</div>
                    <div class="metric-label">Login Success Rate</div>
                </div>
                <div class="metric-card">
                    <div class="metric-value ${stats.wsSuccessRate >= 95 ? 'success' : stats.wsSuccessRate >= 80 ? 'warning' : 'error'}">${stats.wsSuccessRate}%</div>
                    <div class="metric-label">WebSocket Success Rate</div>
                </div>
                <div class="metric-card">
                    <div class="metric-value">${stats.throughput.loginsPerSecond}</div>
                    <div class="metric-label">Logins/Second</div>
                </div>
                <div class="metric-card">
                    <div class="metric-value">${stats.throughput.flagsPerSecond}</div>
                    <div class="metric-label">Flags/Second</div>
                </div>
            </div>

            <!-- Response Time Charts -->
            <div class="chart-container">
                <div class="chart-title">📊 Response Times Over Time</div>
                <div class="chart-wrapper">
                    <canvas id="responseTimeChart"></canvas>
                </div>
            </div>

            <div class="chart-container">
                <div class="chart-title">⚡ Throughput Over Time</div>
                <div class="chart-wrapper">
                    <canvas id="throughputChart"></canvas>
                </div>
            </div>

            <!-- Response Time Statistics -->
            <div class="chart-container">
                <div class="chart-title">📈 Response Time Statistics</div>
                <table>
                    <thead>
                        <tr>
                            <th>Endpoint</th>
                            <th>Mean</th>
                            <th>Median</th>
                            <th>95th %ile</th>
                            <th>99th %ile</th>
                            <th>Min</th>
                            <th>Max</th>
                            <th>Std Dev</th>
                        </tr>
                    </thead>
                    <tbody>
                        <tr>
                            <td>Login</td>
                            <td>${stats.percentiles.login.mean}ms</td>
                            <td><strong>${stats.percentiles.login.median}ms</strong></td>
                            <td>${stats.percentiles.login.p95}ms</td>
                            <td>${stats.percentiles.login.p99}ms</td>
                            <td>${stats.percentiles.login.min}ms</td>
                            <td>${stats.percentiles.login.max}ms</td>
                            <td>${stats.percentiles.login.stdDev}ms</td>
                        </tr>
                        <tr>
                            <td>Flag Submit</td>
                            <td>${stats.percentiles.submit.mean}ms</td>
                            <td><strong>${stats.percentiles.submit.median}ms</strong></td>
                            <td>${stats.percentiles.submit.p95}ms</td>
                            <td>${stats.percentiles.submit.p99}ms</td>
                            <td>${stats.percentiles.submit.min}ms</td>
                            <td>${stats.percentiles.submit.max}ms</td>
                            <td>${stats.percentiles.submit.stdDev}ms</td>
                        </tr>
                        <tr>
                            <td>Challenges API</td>
                            <td>${stats.averageResponseTimes.challenges}ms</td>
                            <td colspan="6" style="text-align: center; color: #666;"><em>Individual timing data not collected</em></td>
                        </tr>
                        <tr>
                            <td>Static Files</td>
                            <td>${stats.averageResponseTimes.staticFiles}ms</td>
                            <td colspan="6" style="text-align: center; color: #666;"><em>Individual timing data not collected</em></td>
                        </tr>
                    </tbody>
                </table>
            </div>

            ${stats.staticFileBreakdown ? `
            <!-- Static File Performance Breakdown -->
            <div class="chart-container">
                <div class="chart-title">📁 Static File Performance Breakdown</div>
                <table>
                    <thead>
                        <tr>
                            <th>File</th>
                            <th>Mean</th>
                            <th>Median</th>
                            <th>95th %ile</th>
                            <th>99th %ile</th>
                            <th>Min</th>
                            <th>Max</th>
                            <th>Std Dev</th>
                            <th>Requests</th>
                        </tr>
                    </thead>
                    <tbody>
                        ${Object.entries(stats.staticFileBreakdown).map(([fileName, stats]) => {
                            const requestCount = fileName === 'index.html' ? metricsData.staticFileBreakdown['index.html'].length :
                                               fileName === 'CSS' ? metricsData.staticFileBreakdown['CSS'].length :
                                               fileName === 'banner image' ? metricsData.staticFileBreakdown['banner image'].length :
                                               fileName === 'app.js' ? metricsData.staticFileBreakdown['app.js'].length : 0;
                            return `<tr>
                                <td>${fileName}</td>
                                <td>${stats.mean}ms</td>
                                <td><strong>${stats.median}ms</strong></td>
                                <td>${stats.p95}ms</td>
                                <td>${stats.p99}ms</td>
                                <td>${stats.min}ms</td>
                                <td>${stats.max}ms</td>
                                <td>${stats.stdDev}ms</td>
                                <td>${requestCount}</td>
                            </tr>`;
                        }).join('')}
                    </tbody>
                </table>
                <p style="margin-top: 15px; color: #666; font-size: 0.9em;">
                    <strong>💡 Tip:</strong> Look for files with high mean/median times or high standard deviation (indicating inconsistent performance).
                    Large images or JavaScript files often take the longest to serve.
                </p>
            </div>
            ` : ''}

            <!-- User Experience Metrics -->
            <div class="chart-container">
                <div class="chart-title">👤 User Experience Metrics</div>

                <!-- UX Summary Cards -->
                <div class="metrics-grid" style="margin-bottom: 20px;">
                    <div class="metric-card">
                        <div class="metric-value">${Math.round(stats.userExperience.timeToFirstLogin.median / 1000)}s</div>
                        <div class="metric-label">Time to First Login (Median)</div>
                    </div>
                    <div class="metric-card">
                        <div class="metric-value">${Math.round(stats.userExperience.timeToFirstFlag.median / 1000)}s</div>
                        <div class="metric-label">Login to First Flag (Median)</div>
                    </div>
                    <div class="metric-card">
                        <div class="metric-value ${stats.userExperience.successRate >= 95 ? 'success' : stats.userExperience.successRate >= 80 ? 'warning' : 'error'}">${stats.userExperience.successRate}%</div>
                        <div class="metric-label">Login Success Rate</div>
                    </div>
                    <div class="metric-card">
                        <div class="metric-value ${stats.userExperience.completionRate >= 95 ? 'success' : stats.userExperience.completionRate >= 80 ? 'warning' : 'error'}">${stats.userExperience.completionRate}%</div>
                        <div class="metric-label">Challenge Completion</div>
                    </div>
                </div>

                <!-- User Journey Breakdown -->
                <table>
                    <thead>
                        <tr>
                            <th>User Journey Stage</th>
                            <th>Count</th>
                            <th>Mean</th>
                            <th>Median</th>
                            <th>95th %ile</th>
                            <th>Min</th>
                            <th>Max</th>
                            <th>Interpretation</th>
                        </tr>
                    </thead>
                    <tbody>
                        <tr>
                            <td>🚪 Time to First Login</td>
                            <td>${stats.userExperience.timeToFirstLogin.count}</td>
                            <td>${Math.round(stats.userExperience.timeToFirstLogin.mean / 1000)}s</td>
                            <td><strong>${Math.round(stats.userExperience.timeToFirstLogin.median / 1000)}s</strong></td>
                            <td>${Math.round(stats.userExperience.timeToFirstLogin.p95 / 1000)}s</td>
                            <td>${Math.round(stats.userExperience.timeToFirstLogin.min / 1000)}s</td>
                            <td>${Math.round(stats.userExperience.timeToFirstLogin.max / 1000)}s</td>
                            <td>From player start to successful login</td>
                        </tr>
                        <tr>
                            <td>🎯 Login to First Flag</td>
                            <td>${stats.userExperience.timeToFirstFlag.count}</td>
                            <td>${Math.round(stats.userExperience.timeToFirstFlag.mean / 1000)}s</td>
                            <td><strong>${Math.round(stats.userExperience.timeToFirstFlag.median / 1000)}s</strong></td>
                            <td>${Math.round(stats.userExperience.timeToFirstFlag.p95 / 1000)}s</td>
                            <td>${Math.round(stats.userExperience.timeToFirstFlag.min / 1000)}s</td>
                            <td>${Math.round(stats.userExperience.timeToFirstFlag.max / 1000)}s</td>
                            <td>Time from successful login to first solve</td>
                        </tr>
                        <tr>
                            <td>🏃 Complete User Journey</td>
                            <td>${stats.userExperience.completeUserJourney.count}</td>
                            <td>${Math.round(stats.userExperience.completeUserJourney.mean / 1000)}s</td>
                            <td><strong>${Math.round(stats.userExperience.completeUserJourney.median / 1000)}s</strong></td>
                            <td>${Math.round(stats.userExperience.completeUserJourney.p95 / 1000)}s</td>
                            <td>${Math.round(stats.userExperience.completeUserJourney.min / 1000)}s</td>
                            <td>${Math.round(stats.userExperience.completeUserJourney.max / 1000)}s</td>
                            <td>Total time from player start to first solve</td>
                        </tr>
                        <tr>
                            <td>⏱️ Session Duration</td>
                            <td>${stats.userExperience.sessionDurations.count}</td>
                            <td>${Math.round(stats.userExperience.sessionDurations.mean / 1000)}s</td>
                            <td><strong>${Math.round(stats.userExperience.sessionDurations.median / 1000)}s</strong></td>
                            <td>${Math.round(stats.userExperience.sessionDurations.p95 / 1000)}s</td>
                            <td>${Math.round(stats.userExperience.sessionDurations.min / 1000)}s</td>
                            <td>${Math.round(stats.userExperience.sessionDurations.max / 1000)}s</td>
                            <td>How long players stay engaged</td>
                        </tr>
                    </tbody>
                </table>

                <!-- Test Performance Insights -->
                <div style="margin-top: 20px; padding: 15px; background: #f8f9fa; border-radius: 8px; border-left: 4px solid #667eea;">
                    <h4 style="margin: 0 0 10px 0; color: #333;">🎯 Test Performance Insights</h4>
                    <ul style="margin: 0; padding-left: 20px; color: #666;">
                        <li><strong>Median Journey:</strong> Typical player takes ${Math.round(stats.userExperience.timeToFirstLogin.median / 1000)}s to login and ${Math.round(stats.userExperience.timeToFirstFlag.median / 1000)}s to reach first flag</li>
                        <li><strong>Login Success:</strong> ${stats.userExperience.successRate}% of players successfully logged in (${stats.userExperience.failedLogins} failures)</li>
                        <li><strong>Test Completion:</strong> ${stats.userExperience.completionRate}% of players completed challenge solving</li>
                        ${stats.userExperience.successRate < 95 ? '<li style="color: #dc2626;"><strong>⚠️ Login Failures:</strong> Server may be struggling with authentication load</li>' : ''}
                        ${stats.userExperience.timeToFirstLogin.p95 > 60000 ? '<li style="color: #dc2626;"><strong>⚠️ Slow Logins:</strong> 95% percentile login time exceeds 60 seconds - check server performance</li>' : ''}
                        ${stats.userExperience.timeToFirstFlag.p95 > 120000 ? '<li style="color: #d97706;"><strong>⚠️ Slow Challenge Loading:</strong> Some players taking >2 minutes to reach first challenge</li>' : ''}
                        ${stats.userExperience.sessionDurations.median < 30000 ? '<li style="color: #d97706;"><strong>⚠️ Short Sessions:</strong> Median session under 30s - possible early failures</li>' : ''}
                    </ul>
                </div>
            </div>

            ${totals.errors.length > 0 ? `
            <!-- Errors Section -->
            <div class="error-section">
                <div class="error-title">⚠️ Errors (${totals.errors.length} total)</div>
                <div class="chart-container">
                    <div class="chart-title">Error Distribution</div>
                    <table>
                        <thead>
                            <tr>
                                <th>Error Type</th>
                                <th>Count</th>
                                <th>Percentage</th>
                            </tr>
                        </thead>
                        <tbody>
                            ${Object.entries(errorsByType).map(([type, count]) =>
                                `<tr>
                                    <td>${type}</td>
                                    <td>${count}</td>
                                    <td>${(count / totals.errors.length * 100).toFixed(1)}%</td>
                                </tr>`
                            ).join('')}
                        </tbody>
                    </table>
                </div>

                <div class="error-title">Recent Errors</div>
                <ul class="error-list">
                    ${totals.errors.slice(0, 20).map(error =>
                        `<li class="error-item">
                            <span class="timestamp">[${new Date(error.timestamp).toLocaleTimeString()}]</span>
                            <strong>${error.context}:</strong> ${error.error}
                            ${error.playerId ? ` (Player ${error.playerId})` : ''}
                        </li>`
                    ).join('')}
                </ul>
                ${totals.errors.length > 20 ? `<p><em>... and ${totals.errors.length - 20} more errors</em></p>` : ''}
            </div>
            ` : '<div class="metric-card" style="margin: 30px auto; max-width: 400px;"><div class="metric-value success">✅</div><div class="metric-label">No errors detected!</div></div>'}
        </div>
    </div>

    <script>
        // Response Time Chart
        const ctx1 = document.getElementById('responseTimeChart').getContext('2d');
        new Chart(ctx1, {
            type: 'line',
            data: {
                labels: ${JSON.stringify(timelineData.labels)},
                datasets: [
                    {
                        label: 'Login Response Time',
                        data: ${JSON.stringify(timelineData.loginTimes)},
                        borderColor: '#667eea',
                        backgroundColor: 'rgba(102, 126, 234, 0.1)',
                        tension: 0.1
                    },
                    {
                        label: 'Submit Response Time',
                        data: ${JSON.stringify(timelineData.submitTimes)},
                        borderColor: '#f093fb',
                        backgroundColor: 'rgba(240, 147, 251, 0.1)',
                        tension: 0.1
                    }
                ]
            },
            options: {
                responsive: true,
                maintainAspectRatio: false,
                scales: {
                    y: {
                        beginAtZero: true,
                        title: {
                            display: true,
                            text: 'Response Time (ms)'
                        }
                    },
                    x: {
                        title: {
                            display: true,
                            text: 'Time'
                        }
                    }
                }
            }
        });

        // Throughput Chart
        const ctx2 = document.getElementById('throughputChart').getContext('2d');
        new Chart(ctx2, {
            type: 'line',
            data: {
                labels: ${JSON.stringify(timelineData.labels)},
                datasets: [
                    {
                        label: 'Active Players',
                        data: ${JSON.stringify(timelineData.activePlayers)},
                        borderColor: '#48bb78',
                        backgroundColor: 'rgba(72, 187, 120, 0.1)',
                        tension: 0.1,
                        yAxisID: 'y'
                    },
                    {
                        label: 'Requests/Second',
                        data: ${JSON.stringify(timelineData.requestsPerSecond)},
                        borderColor: '#ed8936',
                        backgroundColor: 'rgba(237, 137, 54, 0.1)',
                        tension: 0.1,
                        yAxisID: 'y1'
                    }
                ]
            },
            options: {
                responsive: true,
                maintainAspectRatio: false,
                scales: {
                    y: {
                        type: 'linear',
                        display: true,
                        position: 'left',
                        title: {
                            display: true,
                            text: 'Active Players'
                        }
                    },
                    y1: {
                        type: 'linear',
                        display: true,
                        position: 'right',
                        title: {
                            display: true,
                            text: 'Requests/Second'
                        },
                        grid: {
                            drawOnChartArea: false,
                        },
                    },
                    x: {
                        title: {
                            display: true,
                            text: 'Time'
                        }
                    }
                }
            }
        });
    </script>
</body>
</html>`;

    fs.writeFileSync(outputFile, html);
    console.log(`📊 Performance report generated: ${outputFile}`);
}

function calculateUserExperienceMetrics(players, testStartTime) {
    const uxMetrics = {
        timeToFirstLogin: [], // Time from player start to successful login
        timeToFirstFlag: [], // Time from login to first flag submission
        completeUserJourney: [], // Total time from player start to first solve
        challengeSolveTimes: [], // Time to solve each challenge (for all challenges)
        sessionDurations: [], // Total time each player was active
        failedLogins: 0, // Players who failed to login
        incompleteJourneys: 0 // Players who logged in but didn't complete all challenges
    };

    players.forEach(player => {
        if (!player.metrics) return;

        const playerStart = player.metrics.startTime;
        const loginTime = player.metrics.loginTime;
        const firstChallengeTime = player.metrics.firstChallengeTime;
        const challengesSolved = player.metrics.challengesSolved || 0;

        // Time to first login (from when THIS player started, not global test start)
        if (loginTime && playerStart) {
            const ttfl = loginTime - playerStart;
            if (ttfl > 0) uxMetrics.timeToFirstLogin.push(ttfl);
        }

        // User journey timings (from their own start time)
        if (loginTime && firstChallengeTime) {
            const timeToFlag = firstChallengeTime - loginTime;
            const completeJourney = firstChallengeTime - playerStart;

            uxMetrics.timeToFirstFlag.push(timeToFlag);
            uxMetrics.completeUserJourney.push(completeJourney);
        }

        // Failed logins (never got a login time)
        if (!loginTime && playerStart) {
            uxMetrics.failedLogins++;
        }

        // Incomplete journeys (logged in but didn't solve challenges)
        if (loginTime && challengesSolved === 0) {
            uxMetrics.incompleteJourneys++;
        }

        // Session duration
        if (playerStart) {
            const sessionEnd = firstChallengeTime || loginTime || Date.now();
            const sessionDuration = sessionEnd - playerStart;
            uxMetrics.sessionDurations.push(sessionDuration);
        }
    });

    // Calculate summary stats
    return {
        timeToFirstLogin: {
            count: uxMetrics.timeToFirstLogin.length,
            ...calculatePercentiles(uxMetrics.timeToFirstLogin)
        },
        timeToFirstFlag: {
            count: uxMetrics.timeToFirstFlag.length,
            ...calculatePercentiles(uxMetrics.timeToFirstFlag)
        },
        completeUserJourney: {
            count: uxMetrics.completeUserJourney.length,
            ...calculatePercentiles(uxMetrics.completeUserJourney)
        },
        sessionDurations: {
            count: uxMetrics.sessionDurations.length,
            ...calculatePercentiles(uxMetrics.sessionDurations)
        },
        failedLogins: uxMetrics.failedLogins,
        incompleteJourneys: uxMetrics.incompleteJourneys,
        totalPlayers: players.length,
        successRate: players.length > 0 ? ((players.length - uxMetrics.failedLogins) / players.length * 100).toFixed(1) : '0.0',
        completionRate: players.length > 0 ? ((players.length - uxMetrics.incompleteJourneys) / players.length * 100).toFixed(1) : '0.0'
    };
}

function calculateAverage(arr) {
    if (arr.length === 0) return 0;
    return Math.round(arr.reduce((a, b) => a + b, 0) / arr.length);
}

function calculatePercentiles(arr) {
    if (arr.length === 0) return {
        mean: 0, median: 0, p50: 0, p95: 0, p99: 0, max: 0, min: 0, stdDev: 0
    };

    const sorted = arr.slice().sort((a, b) => a - b);
    const mean = arr.reduce((sum, val) => sum + val, 0) / arr.length;

    // Standard deviation
    const variance = arr.reduce((sum, val) => sum + Math.pow(val - mean, 2), 0) / arr.length;
    const stdDev = Math.sqrt(variance);

    return {
        mean: Math.round(mean),
        median: Math.round(sorted[Math.floor(sorted.length * 0.5)]),
        p50: Math.round(sorted[Math.floor(sorted.length * 0.5)]),
        p95: Math.round(sorted[Math.floor(sorted.length * 0.95)]),
        p99: Math.round(sorted[Math.floor(sorted.length * 0.99)]),
        max: Math.round(sorted[sorted.length - 1]),
        min: Math.round(sorted[0]),
        stdDev: Math.round(stdDev)
    };
}

function generateTimelineData(timeline, startTime, endTime) {
    const duration = endTime - startTime;
    const buckets = 20; // 20 time buckets for charts
    const bucketSize = duration / buckets;

    const labels = [];
    const loginTimes = [];
    const submitTimes = [];
    const activePlayers = [];
    const requestsPerSecond = [];

    for (let i = 0; i < buckets; i++) {
        const bucketStart = startTime + (i * bucketSize);
        const bucketEnd = bucketStart + bucketSize;

        // Create time label
        const timeLabel = new Date(bucketStart).toLocaleTimeString();
        labels.push(timeLabel);

        // Filter events in this bucket
        const bucketEvents = timeline.filter(event =>
            event.timestamp >= bucketStart && event.timestamp < bucketEnd
        );

        // Calculate metrics for this bucket
        const loginEvents = bucketEvents.filter(e => e.category === 'login_attempt');
        const submitEvents = bucketEvents.filter(e => e.category === 'submit_flag');

        loginTimes.push(loginEvents.length > 0 ?
            Math.round(loginEvents.reduce((sum, e) => sum + e.value, 0) / loginEvents.length) : 0);

        submitTimes.push(submitEvents.length > 0 ?
            Math.round(submitEvents.reduce((sum, e) => sum + e.value, 0) / submitEvents.length) : 0);

        // Estimate active players (unique player IDs in bucket)
        const uniquePlayers = new Set(bucketEvents.map(e => e.playerId)).size;
        activePlayers.push(uniquePlayers);

        // Requests per second in this bucket
        const requestCount = bucketEvents.length;
        const rps = Math.round(requestCount / (bucketSize / 1000));
        requestsPerSecond.push(rps);
    }

    return { labels, loginTimes, submitTimes, activePlayers, requestsPerSecond };
}

module.exports = { generatePerformanceReport };

// If run directly, expect metrics file as argument
if (require.main === module) {
    const metricsFile = process.argv[2];
    if (!metricsFile) {
        console.error('Usage: node generate-performance-report.js <metrics.json>');
        process.exit(1);
    }

    const metrics = JSON.parse(fs.readFileSync(metricsFile, 'utf8'));
    generatePerformanceReport(metrics);
}