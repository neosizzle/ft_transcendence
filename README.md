# ft_transcendence

![demo](https://github.com/neosizzle/ft_transcendence/blob/main/ft_transcendence.gif)

## Introduction
This is the final project in the 42 Core Program. We are tasked with creating
a working multi-player [Pong](https://en.wikipedia.org/wiki/Pong) game clone
with social media element.

This project is written in Typescript, while relying on React for frontend,
and NestJS and PostgreSQL for backend.

## Starting the server
1. A copy of the environment variable file *.env* needs to be obtained from
on of the developers, and placed in the root folder of the project. This file
contains sensitive information an hence is not uploaded to the git repository.

1. As the project is containerised, we will need to build the docker images
before they can be run. Both of these can be achived by running the following
command while in the project root directory.
    > ```docker-compose up --build```

1. Building the docker image can take awhile. Please be patient and grab a cup
of coffee or tea while you wait.

1. Once everything is up and runnning, you should be able to visit the website
at **http://localhost**.

1. When you are done, you can press Ctrl + C on the terminal or run the
following command to cleanly shutdown the docke`r images:
    > ```docker compose down```

## Troubleshooting
1. If you happen to have docker containers of the same name, the docker image
build will fail. In this case run the following command to remove any unused
docker images and data, before trying to rebuild the images:
    > ```docker system prune```


# Acknowledgement
This project is made with blood, sweat and tears by
- [Edison](https://profile.intra.42.fr/users/edlim) (edlim)
- [Jun Han](https://profile.intra.42.fr/users/jng) (jng)
- [Wee Hean](https://profile.intra.42.fr/users/weng) (weng)
