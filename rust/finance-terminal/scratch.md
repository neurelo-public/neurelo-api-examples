Headers: {"$scalars": true, "$related": true} # This expands the "read your own write"
```
{
    "security_ref": {
        "connect": {
            "id": "66688238ef68e472c67c8d5f"
        }
    },
    "portfolio_ref": {
        "connect": {
            "id": "6668823eef68e472c67c8d60"
        }
    },
    "quantity": 1
}
```

## 
```
curl -X POST "https://us-east-2.aws.neurelo.com/rest/trade/__one?select=%7B%0A++%22%24scalars%22%3A+true%2C%0A++%22%24related%22%3A+true%0A%7D" -H "X-API-KEY:neurelo_9wKFBp874Z5xFw6ZCfvhXZSs8AGi1g1ILQG/JQYrjyNKFMuo8TucCxn4sihu/dvVYB5cVJyAZtdMhPT0JkAa3RzOnPPFOUZX8NB7/DVeSvszEuxatXxVwbdeXQMKCptm6xUE9iFPq9xRhxF/LBozYgxmD4qeQopPt64/WAw4PPD/uEVcBsOhR2JP7WKqizR0_++3NocziTlx/bnGnSKhIvD68nPdQ0p4IkL7dAN3Lunc=" -H "Content-Type:application/json" -H "Accept:application/json"  --data "{\"security_ref\":{\"connect\":{\"id\":\"66688238ef68e472c67c8d5f\"}},\"portfolio_ref\":{\"connect\":{\"id\":\"6668823eef68e472c67c8d60\"}},\"quantity\":1}"
```

## This will get the portfolio that was listed  
```
curl -X GET "https://us-east-2.aws.neurelo.com/rest/portfolio/6668823eef68e472c67c8d60?select=%7B%0A++%22%24scalars%22%3A+true%2C%0A++%22%24related%22%3A+true%0A%7D" -H "X-API-KEY:neurelo_9wKFBp874Z5xFw6ZCfvhXZSs8AGi1g1ILQG/JQYrjyNKFMuo8TucCxn4sihu/dvVYB5cVJyAZtdMhPT0JkAa3RzOnPPFOUZX8NB7/DVeSvszEuxatXxVwbdeXQMKCptm6xUE9iFPq9xRhxF/LBozYgxmD4qeQopPt64/WAw4PPD/uEVcBsOhR2JP7WKqizR0_++3NocziTlx/bnGnSKhIvD68nPdQ0p4IkL7dAN3Lunc=" -H "Content-Type:application/json" -H "Accept:application/json" 
```