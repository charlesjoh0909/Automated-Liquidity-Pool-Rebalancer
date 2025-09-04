# 🔄 Automated Liquidity Pool Rebalancer

[![Clarity](https://img.shields.io/badge/Clarity-Smart%20Contract-blue)](https://clarity-lang.org/)
[![Stacks](https://img.shields.io/badge/Stacks-Blockchain-orange)](https://stacks.co/)

An intelligent automated market maker (AMM) that dynamically rebalances liquidity pools to maintain optimal price ratios and maximize trading efficiency. 💰

## ✨ Features

- 🏊‍♂️ **Automated Liquidity Pools**: Create and manage token pair pools
- 🔄 **Smart Rebalancing**: Automatic price ratio maintenance 
- 💱 **Token Swapping**: Efficient token exchange with minimal slippage
- 📊 **Fee Collection**: Built-in trading fees and protocol revenue
- 🎯 **Target Ratios**: Set desired price targets for optimal trading
- 🛡️ **Slippage Protection**: Minimum output guarantees for all trades

## 🚀 Quick Start

### Prerequisites
- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet for deployment

### Installation
```bash
git clone <repository-url>
cd Automated-Liquidity-Pool-Rebalancer
clarinet check
```

### Deployment
```bash
clarinet deploy
```

## 📋 Usage

### 1. 🏗️ Create a Liquidity Pool

```clarity
(contract-call? .automated-liquidity-pool-rebalancer create-pool 
  'SP2C2YFP12AJZB4MABJBAJ55XECVS7E4PMMZ89YZR.token-a    ; Token X
  'SP2C2YFP12AJZB4MABJBAJ55XECVS7E4PMMZ89YZR.token-b    ; Token Y
  u1000000                                                  ; Initial Token X amount
  u1000000                                                  ; Initial Token Y amount
  u1000000                                                  ; Target price ratio
)
```

### 2. 💧 Add Liquidity

```clarity
(contract-call? .automated-liquidity-pool-rebalancer add-liquidity
  u0          ; Pool ID
  u100000     ; Token X amount
  u100000     ; Token Y amount
  u95000      ; Minimum LP tokens (slippage protection)
)
```

### 3. 🔄 Swap Tokens

**Swap Token X for Token Y:**
```clarity
(contract-call? .automated-liquidity-pool-rebalancer swap-x-for-y
  u0      ; Pool ID
  u10000  ; Token X amount to swap
  u9500   ; Minimum Token Y to receive
)
```

**Swap Token Y for Token X:**
```clarity
(contract-call? .automated-liquidity-pool-rebalancer swap-y-for-x
  u0      ; Pool ID
  u10000  ; Token Y amount to swap
  u9500   ; Minimum Token X to receive
)
```

### 4. 🏃‍♂️ Remove Liquidity

```clarity
(contract-call? .automated-liquidity-pool-rebalancer remove-liquidity
  u0      ; Pool ID
  u50000  ; LP tokens to burn
  u45000  ; Minimum Token X to receive
  u45000  ; Minimum Token Y to receive
)
```

### 5. ⚖️ Trigger Rebalancing

```clarity
(contract-call? .automated-liquidity-pool-rebalancer auto-rebalance u0)
```

## 📊 Read-Only Functions

### Get Pool Information
```clarity
(contract-call? .automated-liquidity-pool-rebalancer get-pool u0)
```

### Get User Position
```clarity
(contract-call? .automated-liquidity-pool-rebalancer get-user-position tx-sender u0)
```

### Get Price Quote
```clarity
(contract-call? .automated-liquidity-pool-rebalancer get-quote-x-for-y u0 u10000)
```

### Check Rebalance Status
```clarity
(contract-call? .automated-liquidity-pool-rebalancer check-rebalance-needed u0)
```

## 🔧 Configuration

### Set Rebalance Threshold (Contract Owner Only)
```clarity
(contract-call? .automated-liquidity-pool-rebalancer set-rebalance-threshold u500)
```

### Set Protocol Fee Rate (Contract Owner Only)
```clarity
(contract-call? .automated-liquidity-pool-rebalancer set-protocol-fee-rate u30)
```

## 🧮 How It Works

1. **Pool Creation**: Users create token pairs with initial liquidity and target ratios
2. **Trading**: Users can swap tokens with automatic fee collection
3. **Auto-Rebalancing**: When price deviates from target ratio beyond threshold, the system automatically rebalances
4. **LP Rewards**: Liquidity providers earn fees proportional to their pool share

## 💡 Key Concepts

- **LP Tokens**: Represent your share of the liquidity pool
- **Target Ratio**: Desired price ratio that triggers rebalancing
- **Slippage**: Price impact from large trades
- **Fees**: Trading fees distributed to liquidity providers

## ⚠️ Error Codes

- `u100`: Not authorized
- `u101`: Insufficient liquidity
- `u102`: Invalid amount
- `u103`: Pool doesn't exist
- `u104`: Slippage too high
- `u105`: Already exists
- `u106`: Zero amount

## 🧪 Testing

```bash
clarinet test
```

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch
3. Commit your changes
4. Push to the branch
5. Create a Pull Request

## 📄 License

MIT License - see the LICENSE file for details

---

**Built with ❤️ for the Stacks ecosystem** 🔥
