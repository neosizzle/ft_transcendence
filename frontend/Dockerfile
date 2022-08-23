# #base image
# FROM node

# RUN mkdir /install

# #install dependencies global
# RUN npm install -g react-scripts

# ADD . /ft_transcendence-front
# WORKDIR /ft_transcendence-front

# #expose port
# EXPOSE 3000

# #start app
# CMD ["bash", "prod.sh"]


# ==== CONFIGURE =====
# Use a Node 16 base image
FROM node
# Set the working directory to /app inside the container
WORKDIR /app
# Copy app files
COPY . .
# ==== BUILD =====
# Install dependencies (npm ci makes sure the exact versions in the lockfile gets installed)
RUN npm ci 
# Build the app
RUN npm run build
# ==== RUN =======
# Set the env to "production"
ENV NODE_ENV production
# Expose the port on which the app will be running (3000 is the default that `serve` uses)
EXPOSE 3000
# Start the app
CMD [ "npx", "serve", "-s", "build" ]




# #base image
# FROM node

# # Create app directory
# WORKDIR /ft_transcendence-front

# # Install app dependencies
# # A wildcard is used to ensure both package.json AND package-lock.json are copied
# # where available (npm@5+)
# COPY package*.json ./

# RUN npm install --legacy-peer-deps
# # If you are building your code for production
# # RUN npm ci --only=production

# # Bundle app source
# COPY . .

# EXPOSE 3000
# CMD [ "bash", "prod.sh" ]