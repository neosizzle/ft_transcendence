import React, { FunctionComponent, useState } from "react";
import { OUTGOING_CREATE } from "../../constants";
import { useAuth } from "../../context/authContext";
import { roomDto } from "../classes";

const AddIcon: FunctionComponent = () => {
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      fill="none"
      viewBox="0 0 24 24"
      strokeWidth={1.5}
      stroke="currentColor"
      className="mx-auto mb-4 text-gray-400 w-14 h-14 dark:text-gray-200"
    >
      <path
        strokeLinecap="round"
        strokeLinejoin="round"
        d="M12 9v6m3-3H9m12 0a9 9 0 11-18 0 9 9 0 0118 0z"
      />
    </svg>
  );
};

interface CreateRoomModalProps {
  setOpenCreateRoomModal: React.Dispatch<React.SetStateAction<boolean>>;
}

const CreateRoomModal: FunctionComponent<CreateRoomModalProps> = ({
  setOpenCreateRoomModal,
}) => {
  const [roomNameInput, setRoomNameInput] = useState<string>("");
  const [roomNameErr, setRoomNameErr] = useState<boolean>(false);
  const [passwordInput, setPasswordInput] = useState<string>("");
  const [passwordErr, setPasswordErr] = useState<boolean>(false);
  const [confirmPasswordInput, setConfirmPasswordInput] = useState<string>("");
  const [confirmPasswordErr, setConfirmPasswordErr] = useState<boolean>(false);
  const auth = useAuth();

  return (
    <div className="fixed top-0 left-0 right-0 z-50 h-full overflow-x-hidden overflow-y-auto md:inset-0  bg-gray-300/50">
      <div className="relative w-full p-4 flex justify-center item-center">
        <div className="relative bg-white rounded-lg shadow dark:bg-gray-700">
          <button
            onClick={() => {
              setOpenCreateRoomModal(false);
            }}
            type="button"
            className="absolute top-3 right-2.5 text-gray-400 bg-transparent hover:bg-gray-200 hover:text-gray-900 rounded-lg text-sm p-1.5 ml-auto inline-flex items-center dark:hover:bg-gray-800 dark:hover:text-white"
            data-modal-toggle="popup-modal"
          >
            <svg
              className="w-5 h-5"
              fill="currentColor"
              viewBox="0 0 20 20"
              xmlns="http://www.w3.org/2000/svg"
            >
              <path
                fillRule="evenodd"
                d="M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z"
                clipRule="evenodd"
              ></path>
            </svg>
          </button>
          <div className="p-6 text-center">
            <AddIcon />
            <h3 className="mb-5 text-lg font-normal text-gray-500 dark:text-gray-400">
              Create a new room
            </h3>

            {/* Inputs*/}
            <input
              type="text"
              value={roomNameInput}
              onChange={(e) => setRoomNameInput(e.target.value)}
              className={`mb-2 bg-gray-50 border ${
                roomNameErr
                  ? "border-red-500 text-red-500 focus:ring-red-500 focus:border-red-500 focus:outline-red-500"
                  : "border-gray-300 text-gray-900 focus:ring-gray-500 focus:border-gray-500 "
              } text-sm rounded-lg block w-full p-2.5 `}
              placeholder="Room name"
            />
            {roomNameErr ? (
              <div className="text-sm mb-2 text-red-500">
                This field is required
              </div>
            ) : null}

            <input
              type="password"
              value={passwordInput}
              onChange={(e) => setPasswordInput(e.target.value)}
              className={`mb-2 bg-gray-50 border ${
                passwordErr
                  ? "border-red-500 text-red-500 focus:ring-red-500 focus:border-red-500 focus:outline-red-500"
                  : "border-gray-300 text-gray-900 focus:ring-gray-500 focus:border-gray-500 "
              } text-sm rounded-lg block w-full p-2.5 `}
              placeholder="Room password (Optional)"
            />
            {passwordErr ? (
              <div className="text-sm mb-2 text-red-500">
                Password needs to be at least 7 characters long
              </div>
            ) : null}

            <input
              type="password"
              value={confirmPasswordInput}
              onChange={(e) => setConfirmPasswordInput(e.target.value)}
              className={`mb-2 bg-gray-50 border ${
                confirmPasswordErr
                  ? "border-red-500 text-red-500 focus:ring-red-500 focus:border-red-500 focus:outline-red-500"
                  : "border-gray-300 text-gray-900 focus:ring-gray-500 focus:border-gray-500 "
              } text-sm rounded-lg block w-full p-2.5 `}
              disabled={passwordInput.length < 1}
              placeholder="Confirm Password"
            />
            {confirmPasswordErr ? (
              <div className="text-sm mb-2 text-red-500">
                Password does not match
              </div>
            ) : null}

            <button
              onClick={() => {
                // set loading state to true

                setPasswordErr(false);
                setConfirmPasswordErr(false);
                setRoomNameErr(false);
                // validate input
                if (roomNameInput.length < 1) {
                  setRoomNameErr(true);
                  return;
                }
                if (passwordInput.length > 0 && passwordInput.length < 7) {
                  setPasswordErr(true);
                  return;
                }
                if (
                  passwordInput.length > 0 &&
                  passwordInput !== confirmPasswordInput
                ) {
                  setConfirmPasswordErr(true);
                  return;
                }

                // build body
                const payload: roomDto = {
                  roomName: roomNameInput,
                  type: "GC",
                };
                if (passwordInput.length > 0) payload.password = passwordInput;

                // emit ws create event
                auth?.chatSocket?.emit(
                  OUTGOING_CREATE,
                  JSON.stringify(payload)
                );

                // close window
                setOpenCreateRoomModal(false);
              }}
              type="button"
              className={`w-full text-gray-500 bg-white hover:bg-gray-100 focus:ring-4 focus:outline-none focus:ring-gray-200 rounded-lg border border-gray-200 text-sm font-medium px-5 py-2.5 hover:text-gray-900 focus:z-10 dark:bg-gray-700 dark:text-gray-300 dark:border-gray-500 dark:hover:text-white dark:hover:bg-gray-600 dark:focus:ring-gray-600`}
            >
              Create room
            </button>
            <button
              onClick={() => {
                // if (loading) return;
                setOpenCreateRoomModal(false);
              }}
              type="button"
              className={`w-full mt-2 text-gray-500 bg-white hover:bg-gray-100 focus:ring-4 focus:outline-none focus:ring-gray-200 rounded-lg border border-gray-200 text-sm font-medium px-5 py-2.5 hover:text-gray-900 focus:z-10 dark:bg-gray-700 dark:text-gray-300 dark:border-gray-500 dark:hover:text-white dark:hover:bg-gray-600 dark:focus:ring-gray-600`}
            >
              Cancel
            </button>
          </div>
        </div>
      </div>
    </div>
  );
};

export default CreateRoomModal;
