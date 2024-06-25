RUN_BIN :=./target/debug/neurelo-rust-terminal
TERM_BIN :=./target/debug/neurelo-rust-terminal

build: 
	@cargo build --quiet

create-portfolio: build
	@$(RUN_BIN) run create portfolio --name 'Umbrella Corp' | jq .

create-security: build
	@$(RUN_BIN) run create security --company-name 'Wonka Enterprises' --ticker-symbol 'WNKA' | jq .

get-portfolio: build
	@$(RUN_BIN) run get portfolio --id 6675f131b3e5fe8dcb0c7040 | jq .

get-security: build
	@$(RUN_BIN) run get security --id 66688238ef68e472c67c8d5f | jq .

get-trade: build
	@$(RUN_BIN) run get trade --id 666c565dfec5bada02dff686 | jq .

create-trade: build
	@$(RUN_BIN) run create trade --quantity 10 --action "BUY" --price 98.80 --portfolio-id 6675f131b3e5fe8dcb0c7040 --security-id 6675f1bbb3e5fe8dcb0c7042 | jq .

reset: build
	@$(RUN_BIN) run delete-all trade
	@$(RUN_BIN) run delete-all portfolio
	@$(RUN_BIN) run delete-all security

tui: build
	@$(RUN_BIN) terminal

