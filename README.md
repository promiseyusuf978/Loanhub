# Loanhub

# 🏦 Loanhub - Community Lending Pools

> 🌾 Empowering rural entrepreneurs through pooled micro-lending on Stacks blockchain

## 📖 Overview

Loanhub is a decentralized lending platform that enables communities to create lending pools for micro-financing rural entrepreneurs. Pool members contribute STX tokens and earn interest from loans, while borrowers access affordable credit with flexible terms.

## ✨ Features

- 🏗️ **Create Lending Pools** - Start community-driven lending initiatives
- 🤝 **Join Existing Pools** - Contribute to established lending communities  
- 💰 **Request Loans** - Access micro-credit with competitive rates
- 🔒 **Collateral Support** - Optional collateral for loan security
- 📊 **Transparent Terms** - Clear interest rates and repayment schedules
- 💸 **Flexible Withdrawals** - Withdraw contributions when funds are available

## 🚀 Getting Started

### Prerequisites
- Clarinet CLI installed
- Stacks wallet with STX tokens

### Installation

```bash
git clone <repository-url>
cd loanhub
clarinet check
```

## 🔧 Usage

### Creating a Lending Pool

```clarity
(contract-call? .Loanhub create-lending-pool 
  "Rural Farmers Pool" 
  u10          ;; 10% interest rate
  u1000000     ;; 1 STX max loan amount  
  u144         ;; 144 blocks loan duration
  u500000)     ;; 0.5 STX initial contribution
```

### Joining a Pool

```clarity
(contract-call? .Loanhub join-pool 
  u1           ;; pool-id
  u1000000)    ;; 1 STX contribution
```

### Requesting a Loan

```clarity
(contract-call? .Loanhub request-loan 
  u1           ;; pool-id
  u500000      ;; 0.5 STX loan amount
  u100000)     ;; 0.1 STX collateral (optional)
```

### Repaying a Loan

```clarity
(contract-call? .Loanhub repay-loan u1) ;; loan-id
```

## 📋 Contract Functions

### Public Functions
- `create-lending-pool` - Create a new lending pool
- `join-pool` - Join an existing pool with contribution
- `request-loan` - Request a loan from a pool
- `repay-loan` - Repay an outstanding loan
- `withdraw-contribution` - Withdraw funds from pool

### Read-Only Functions  
- `get-pool-info` - Get pool details
- `get-loan-info` - Get loan information
- `get-member-info` - Get member contribution details
- `is-pool-member` - Check pool membership
- `calculate-loan-repayment` - Calculate total repayment amount
- `is-loan-overdue` - Check if loan is past due date

## 🔍 Example Queries

### Check Pool Information
```clarity
(contract-call? .Loanhub get-pool-info u1)
```

### Calculate Loan Repayment
```clarity
(contract-call? .Loanhub calculate-loan-repayment u1)
```

### Check Membership Status
```clarity
(contract-call? .Loanhub is-pool-member u1 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
```

## 💡 Key Benefits

- 🌍 **Community-Driven** - Pools managed by community members
- 💎 **Low Barriers** - Accessible micro-lending for small entrepreneurs  
- 🔐 **Blockchain Security** - Transparent and immutable transactions
- 📈 **Earn Interest** - Pool contributors earn from loan repayments
- 🏘️ **Local Focus** - Designed for rural and underserved communities

## ⚠️ Important Notes

- Pool members must contribute before requesting loans
- Interest rates are set per pool and applied to all loans
- Collateral is optional but recommended for larger loans
- Loans have fixed duration terms set by pool creators
- Overdue loans can be identified using read-only functions

## 🤝 Contributing

We welcome contributions! Please feel free to submit issues and pull requests.

## 📄 License

This project is open source and available under the MIT License.


