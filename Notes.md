# ft_trancendence: A Not-So-Brief Introduction 

## Infrastructure and architecture design
The infrastructure of this project is set by the subject, so there is not much room to wiggle for this one. According the the subject, We are required to use
docker compose for ALL application services (API, game server, frontend, any jobs etc etc). For the stack of frontend, we are required to use any TYPESCRIPT framework. We went with [React](https://www.typescriptlang.org/docs/handbook/react.html). For the backend API, we are required to use [NestJs](https://nestjs.com/). For the backend database, we have to use [A Postgres database](https://github.com/porsager/postgres). Of course, how we integrate this into our project is entirely up to us.

We set up 3 containers, (frontend, backend, db) and connect them to the same network. We only expose ports which are required for the system to interact with the outside world (HTTP) which is port 80. The internal services can communicate w/ each other using the network set up above isolated from the public internet.

We also have a utility script that runs every now and then (userStatus.sh) which checks for user status. We run that script during the building of the backend API image in the background. It is something like a cloud function if you are developing in a cloud environment. 

We also use git for version control, github issues for bug tracking and github projects for task updates and general management. We also used figma for screen prototyping and whiteboarding.

## Security concerns
Security is also a requirement to validate this project. In simple terms, if any user accessed (CRUD) items which my theory is not allowed to, your system is vulnerable to that said user and he can audit the system to serve his malicious purposes with or without the knowldge of the devloper.

Here are the list of security concerns mentioned by the subject and the way to overcome said concerns.
1. Passwords must be hashed (Before storing password in database, hash it using a 1 way hasing algorithm so that developers wont know user passwords)
2. SQL injection must be prevented (Use an ORM. If you dont want to use ORMs, you will need to sanitize user input and use prepared statements for database queries)
3. Docker must not run in bind mount volume (The souce code must not be linked in any way to the running application. The code must be COMPILED then SERVED seperately. that way if the source code is changed by bad actor, the application would not apply the changes)

## Authentication and User module
Although these two are seperate requirements, I grouped them together since they are closely tied to each other and one depends on the other. 
For a start, a user is defined in our system as a valid, authenticated user from 42 intranet. If you dont have an intra account, you cant be a user of our system. Therefore, we have 2 layers of authntication in place, our backend layer and 42 intranets layer. We wont be discussing about the intranets authntication layer since we are not the ones what implemented it, this will only cover the authentication for the nestjs backend layer.

Just like intranet, we will also be using the Oauth token system (infact, the tokens we issue is the same tokens intranet issues) to authorize and authenticate users. Authorizing users grant the user permission to access resources, Authenticating users will identify the user who are accessing resources. The high level of a login flow would be something like this in our system.

User logs in via intra @ frontend -> intra will generate a code for us (NOT OAUTH TOKEN), we send code to backend -> backend calls 42 api to verify code and get token -> we get the user info with the token, store the token and user on our own database.

You might ask, why do we store the copy of the same exact token and user in our database when we can use 42 API anyways? The students doing this project lives far away from france, and the time to get the API response can go up to 500ms. Storing a cached copy in the database can reduce that overhead and provide a better user experience. We also need to store the user as a seprate entity anyways so it would be better off storing the auth credential seperately as well

Of couse, we will also be implementing our own user objects on top of the user object provided by 42 API. Our user objects have properties of 
- Username 
- Intra name
- Intra Id
- Avatar
- Level
- Status (ONLINE, OFFLINE, INGAME)
- Rank
- Wins
- Losses 
- Relationships with other models (will get to this soon)

these prpoperties are the base properties that our user module will have and it would get created upon signup.

## Friends and blocks
Friends and blocks are both relations that can happen from many users to many users. Meaning that user A can both befriend/block user B, user A can both befreiend/block user C, user B can both befriend/block user C and so on... This would mean that we need to store an array of users in the Users model to record all these relationships. This can be easily done in a noSQL database however since we are using postgres, we need to have another table, lets call it friendship. This table acts as a lookup table (it does not contain any metadata, just contains 2 columns :- the user and the befriended/blocked user). The blocks table will have the same functionality as well. 

To add a friend or a block, a POST request is done to the resource and the record is added to the friendship or blocks table resulting in a new block.

To retreive all friends / blocks from a certain user, we need to filter the friendship/blocks database to return rows that contain the userid if the desired user.

Of course, more complicated filters suck as filtering by friends by username, filtering friends by level can also be done using nested queries. Just need to spend some more time to code that one out (wont contrubute anything to eval but its worth a try)

## Pong Game
The most basic implementation of this game involves 1 ball and 2 Paddles:
1. The ball reflects after colliding with the paddles or touching the top / bottom boundary. 
1. The paddle movement can be controlled by the user via specific keys. However, they cannot travel beyond the top / bottom boundary.

It may seem that the collision mechanics of these different entities are very different and require different implementation. However, it's possible to differentiate the collision mechanics into two different parts:
- Collision detection: how the collision between 2 objects are detected. This should preferably be the same across different objects.
- Collision reaction: how an object behaves when collision occurs. Different objects can behave differently upon collision, e.g. ball reflects, paddles stop moving.

Furthermore, we can simplify the mechanics further by introducing immovable wall objects located at the top and bottom of the boundary. When the ball collides with a wall, it will reflect as intended. When a paddle collides with a wall, it will stop moving. In our implementation, when a paddle collides with the ball, it will stop moving too, but since that is likely to happen for a frame only, it does not have any noticeable impact on the game.

### Collision Detection
A simple collision detection method is via the 2D bounding box collision detection. In the method, every object has a 2D bounding box surrounding them, and collision happens when bounding boxes overlap. We'll let the reader conduct their own research on how this method is implemented.

An important issue to take note is that not only do we have to detect the occurrence of a collision, we also need to identify the side(s) of collision, as that may affect how the object reacts upon collision, e.g. ball will reflect towards the bottom if the collision is at the top.

Another consideration is what would happen if collisions occur for opposite sides of an object. We'll let the reader ponder about this issue, and how it could be handled.

### Collision Reaction
To keep track of the location and movement of objects, we will need to record their x-coordinate, y-coordinate, x-speed and y-speed.

- **Ball**: The ball reflects (speed changes sign) depending on the side of collision. For example, upon collision at the top or bottom, the sign of y-speed will be reversed. Similarly for x-speed upon collision of at the left or right.

- **Paddle**: Paddles simply stop moving along a particular direction depending on the side of collision. For example, upon collision at the top, y-speed will be set to 0 to prevent further movement towards the top. Notice, however, if implemented incorrectly, a paddle may *stick* to a wall upon collision, and unable to *unstick* itself. The solution to this is left as an exercise to the reader.

- **Wall**: It's immovable, so there's no reaction.

Notice that in the original Pong, the ball speeds up as the round of game progresses. There are a few ways this could be implemented, some easier than others.

### Game Display
There are a few ways to display the game on the browser, but HTML Canvas is perhaps the most direct method. In draw the game onto the canvas, the following needs to be done:

1. Detect collision for all objects.
1. If collision is detected, the object will react according to some predefined behaviour.
1. Update the location of all the objects.
1. Draw all the objects onto the canvas using the canvas context.

Something important to take note is that each frame is animated by calling the `requestAnimationFrame` function. However, different monitors / display devices may have different refresh rates. For example, a typical computer monitor may have a refresh rate of 60Hz, but higher end gaming monitors may have 120 Hz or more. If not implemented correctly, players with monitors of different refresh rates may see different states of the game purely because the game is not running at the same speed for each of them.

## Multi-Player Game
Since we are creating a multiplayer game to be played by different players using different devices, this necessitates a game server. Simplistically, few things need to happen between Clients and Server:
- Clients update the Server upon new events
- Server takes these events into consideration and updates its game state. 
- For various reasons, the game state of different Clients may different from that of the Server. The game state of the server is thus considered the canonical version.
- The Server updates all Clients of the latest game state.
- Upon receipt of the new game state, the Clients synchronise their game states to that of the server's. 

### Handling Network Latency
It is a physical impossibility to exceed the cosmic speed limit, i.e. the speed of light. In terms of communication via a network, there are other sources that slow down the communication. Nevertheless, for this project, we can simplistically classify the latency into 2 categories:
- Client-to-Server latency: This is the time taken for information to travel from a Client to the Server.
- Server-to-Client latency: This is the time taken for information to travel from the Server to a Client.

There are many ways Clients and Server can detect the magnitude of latency in the network. However, one simplistic but highly inefficient method is to have the Clients ping the Server measuring the time taken for a response to be received. The Client-to-Server and Server-to-Client latency is assumed to be equal.

When a signal / information is received by the Server from a Client, it *may* take the Client-to-Server latency into consideration and do the necessary update. However, this is not implemented in our project due to certain reasons that we will not elaborate here.

One key method in handling network latency is to have the Server and Clients running their own instance of the game. This is because a player expects the game to respond very quickly upon an input event. If the Clients have to wait for the Server to send an updated game state for every frame, it will render the game practically unplayable.  

Once a Client receives a game state from the Client, it must take the Server-to-Client latency into consideration. Otherwise, the objects in the game may appear to travel back in time as the Client game state is synchronised to the Server game state that is from some time in the past. One simple way is to take into consideration how many additional frames need to be updated, depending on the game frame rate and magnitude of Server-to-Client latency. 

Furthermore, Server will also need to update its canonical game state to every client so that all the clients will see mostly the same game state. Otherwise, the game states between different Clients may drift apart and the players may experience a teleportation effect when the objects in the game suddenly jump from one location to another. Careful consideration also needs to be taken on how often the canonical game states should be sent to clients.

Before we end the section, we should mention that the communication between game Clients and game Server could be implemented using multiple methods. We have chosen to use WebSocket for this purpose. As the same technology is used for different parts of the project, we will not be elaborating on the use of WebSocket in this section.

### Game Speed Synchronisation
As have been mentioned in the previous section, each Client and Server will run their own copy of the game. The Clients may call `requestAnimationFrame` to animate the game, whereas the Server may call `setInterval` instead. 

Due to `requestAnimationFrame` refreshing the game at different rates depending on the refresh rate of the monitor, it is necessary to throttle or hasten the speed of a game, depending on the actual game frame rate.

The following method is a general recommendations to mitigate the issue:
1. Since a 60 Hz monitor is the norm, we can decide that the game should refresh at 60 Hz. The Server can pass in the appropriate parameter to `setInterval` to achieve this.
1. For Clients, since the refresh rate may vary, we can instead calculate how much time has elapsed since the previous frame update to determine how many *steps* we need to move ahead in order to keep the game in sync with the Server.

Take note, however, how smooth the display is highly dependent on the refresh rate of the monitor. This is the reason why gamers may prefer monitors with high refresh rate. Some phones, for example, may have a lower refresh rate of 48 Hz and the display will definitely feel less smooth when viewed on such devices.
