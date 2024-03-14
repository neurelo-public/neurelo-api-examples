# SpringBoot JAVA - Neurelo API

- This is a simple spring boot app that demonstrates how to use the neurelo api to create, search, view single and update film.
- This example use [Thymeleaf](https://www.thymeleaf.org/) templating engine to render the views/pages.
- HTTP requests to Neurelo API are made using std HttpClient package.

### Prerequisites:

- OpenJDK 21 or higher [ [Download From Source](https://openjdk.java.net/) or [Install using Brew](https://formulae.brew.sh/formula/openjdk) ]
- Gradle 8.6 or higher [ [Download From Source](https://gradle.org/releases/) or [Install using Brew](https://formulae.brew.sh/formula/gradle) ]
- SpringBoot CLI v3.2.2 or higher [ [Download From Source](https://docs.spring.io/spring-boot/docs/current/reference/html/cli.html) ]

> :warning: **Note**: Alternatively you can use maven to build and run the application.

### Run the application:

- Clone the repository
- CD into `java/springboot`

```sh
cd ./java/springboot
```

- Modify `application.properties` to match your neurelo api configuration
- Change permissions for `gradlew` by running:

```sh
chmod +x ./gradlew
```

- Start the application by running:

```sh
./gradlew bootRun --no-build-cache
```

### View:

- Open your browser and navigate to [http://localhost:8080](http://localhost:8080)
