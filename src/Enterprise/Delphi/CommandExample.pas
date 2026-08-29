program CommandExample;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Generics.Collections;

type
  TAccount = class
  private
    FBalance: Integer;
  public
    constructor Create(InitialBalance: Integer);
    procedure Deposit(Amount: Integer);
    procedure Withdraw(Amount: Integer);
    property Balance: Integer read FBalance;
  end;

  ICommand = interface
    ['{2A9C80D5-D88E-40CF-A411-53EA32C94A59}']
    procedure Execute;
  end;

  TDepositCommand = class(TInterfacedObject, ICommand)
  private
    FAccount: TAccount;
    FAmount: Integer;
  public
    constructor Create(Account: TAccount; Amount: Integer);
    procedure Execute;
  end;

  TWithdrawCommand = class(TInterfacedObject, ICommand)
  private
    FAccount: TAccount;
    FAmount: Integer;
  public
    constructor Create(Account: TAccount; Amount: Integer);
    procedure Execute;
  end;

constructor TAccount.Create(InitialBalance: Integer);
begin
  inherited Create;
  FBalance := InitialBalance;
end;

procedure TAccount.Deposit(Amount: Integer);
begin
  Inc(FBalance, Amount);
end;

procedure TAccount.Withdraw(Amount: Integer);
begin
  Dec(FBalance, Amount);
end;

constructor TDepositCommand.Create(Account: TAccount; Amount: Integer);
begin
  inherited Create;
  FAccount := Account;
  FAmount := Amount;
end;

procedure TDepositCommand.Execute;
begin
  FAccount.Deposit(FAmount);
end;

constructor TWithdrawCommand.Create(Account: TAccount; Amount: Integer);
begin
  inherited Create;
  FAccount := Account;
  FAmount := Amount;
end;

procedure TWithdrawCommand.Execute;
begin
  FAccount.Withdraw(FAmount);
end;

var
  Account: TAccount;
  Queue: TList<ICommand>;
  Command: ICommand;
begin
  Account := TAccount.Create(100);
  Queue := TList<ICommand>.Create;
  try
    Queue.Add(TDepositCommand.Create(Account, 50));
    Queue.Add(TWithdrawCommand.Create(Account, 20));
    for Command in Queue do
      Command.Execute;
    Assert(Account.Balance = 130);
    Writeln(Format('balance=%d;commands=%d', [Account.Balance, Queue.Count]));
  finally
    Queue.Free;
    Account.Free;
  end;
end.
