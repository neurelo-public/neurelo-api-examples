RUN_BIN :=./target/debug/neurelo-rust-example
TERM_BIN :=./target/debug/neurelo-rust-example

build: 
	@cargo build --quiet

create-portfolio: build
	@$(RUN_BIN) run create portfolio --name 'Umbrella Corp' | jq .

get-portfolio: build
	@$(RUN_BIN) run get portfolio --id 6668823eef68e472c67c8d60 | jq .

get-security: build
	@$(RUN_BIN) run get security --id 66688238ef68e472c67c8d5f | jq .

get-trade: build
	@$(RUN_BIN) run get trade --id 666c565dfec5bada02dff686 | jq .

create-trade: build
	@$(RUN_BIN) run create trade --quantity 10 --action "BUY" --price 98.80 --portfolio-id 666c5d62aa6ced35d515fdf6 --security-id 666c5d63aa6ced35d515fdfa | jq .

reset: build
	@$(RUN_BIN) run delete-all trade
	@$(RUN_BIN) run delete-all portfolio
	@$(RUN_BIN) run delete-all security

tui: build
	@$(RUN_BIN) terminal

