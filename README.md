# Haskell DynamoDB Example

Set up:

- nodejs is required because Serverless framework for Haskell is used
- `yarn install` / `npm install` to install dependencies
- `sls dynamodb install` to install DynamoDB local
- run `yarn dynamo` to start DynamoDB local

---

Commands:

- `yarn/npm run dev:get`: get order by order id
- `yarn/npm run dev:getByUser`: get orders by userId
- `yarn/npm run dev:create`: create order
- `yarn/npm run dev:update`: update order status

(see package.json)

For local development, fake API Gateway event and fake credentials in .env.aws-example are used.
All functions for serverless are in `app` folder.

---

Note:

- Sending query to DynamoDB:
  - You need to have DynamoDB env, which you can get from `newEnv`
  - Then, to send a query to DynamoDB you need to run `runResourceT . runAWST env $ send <query operation>`

* When you run `yarn dynamo`, DynamoDB local starts running and seed file is run too. See `.seeds/orders.json` file.

* For more information about Serverless, check https://www.serverless.com/framework/docs/providers/aws/cli-reference/
